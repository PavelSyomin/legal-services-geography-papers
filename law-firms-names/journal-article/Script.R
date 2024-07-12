library(dplyr)
library(ggplot2)
library(glue)
library(here)
library(readr)
library(readxl)
library(sf)
library(stringr)
library(tidyr)

data <- read_csv(here("../../ru-smb-companies/legal/panel.csv"))
er <- read_csv(here("journal-article", "economic-regions.csv"))
tiles <- read_csv(here("common", "russia-tiles.csv"))
ru <- st_read(here("common", "ru.geojson"))
ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")
org_forms <- c(
  "общество с ограниченной ответственностью",
  "общество с дополнительной ответственностью",
  "акционерное общество",
  "открытое акционерное общество",
  "закрытое акционерное общество",
  "публичное акционерное общество",
  "непубличное акционерное общество",
  "хозяйственное партнерство",
  "полное товарищество",
  "товарищество на вере"
)

firms <- data %>% 
  filter(
    kind == 1, # companies only
    year <= 2021, # 2016–2021
  ) %>% 
  mutate(
    name_id = row_number(),
    settlement = case_when(
      region == "Москва" ~ "Москва",
      region == "Санкт-Петербург" ~ "Санкт-Петербург",
      TRUE ~ settlement)
  ) %>% 
  select(
    name_id,
    tin,
    name = org_name, 
    region, 
    settlement, 
    lat,
    lon, 
    year
  ) %>% 
  mutate(
    name = str_to_lower(name),
    name = str_remove(name, paste0(org_forms, collapse = "|")),
    name = str_remove_all(name, fixed("\"")),
    name = str_trim(name)
  )
unique_names <- distinct(firms, name)

# Save unique names to a file for processing with YandexGPT API
write_csv(
  unique_names,
  glue("names-{strftime(Sys.time(), '%Y-%m-%d-%H-%M-%S')}.csv")
)

# Load vectors obtained from YandexGPT API
vectors <- read_csv(here("common", "names-vectors.csv"))

# Clustering
## Find optimal number of clusters
n_clusters <- 2:100
scores <- sapply(
  n_clusters, 
  function(x) kmeans(
    slice_sample(select(vectors, dim_0:dim_255), n = 1000),
    centers = x)$tot.withins
)
ggplot(tibble(x = n_clusters, y = scores), aes(x = x, y = y)) +
  geom_line()

# Cluster names and save a sample for manual analysis
set.seed(42)
fit <- kmeans(select(vectors, dim_0:dim_255), centers = 50)
vectors$cluster <- fit$cluster
vectors %>% 
  group_by(cluster) %>% 
  slice_sample(n = 20) %>% 
  select(name) %>% 
  write_csv(
    glue("names-sample-{strftime(Sys.time(), '%Y-%m-%d-%H-%M-%S')}.csv")
  )

# Load the results of manual analysis and join them with names
cluster_labels <- read_excel(here("journal-article", "cluster-labels.xlsx"))
firms_lifetime <- count(firms, tin, name = "lifetime")
clustered <- vectors %>% 
  left_join(cluster_labels) %>% 
  right_join(firms) %>% 
  right_join(firms_lifetime) %>% 
  filter(name != "", lifetime >= 3) %>% 
  select(
    name_id, tin, name, region, settlement,
    lat, lon, year, 
    cluster, keywords, tag, group, strategy, western) %>% 
  drop_na(name, tin, region, settlement, lat, lon, year) %>% 
  distinct(name, tin, .keep_all = TRUE)

# Distance between regions
regions_cnt <- vectors %>% 
  right_join(firms) %>% 
  right_join(firms_lifetime) %>% 
  filter(name != "", lifetime >= 3) %>% 
  drop_na(region) %>% 
  distinct(name, tin, .keep_all = TRUE) %>% 
  count(region, sort = TRUE)
quantile(regions_cnt$n, .05)
region_vectors <- vectors %>% 
  right_join(firms) %>% 
  right_join(firms_lifetime) %>% 
  filter(name != "", lifetime >= 3) %>% 
  distinct(name, tin, .keep_all = TRUE) %>% 
  drop_na(region) %>% 
  group_by(region) %>% 
  summarise(across(dim_0:dim_255, mean), cnt = n()) %>% 
  filter(cnt > quantile(cnt, .05)) %>% 
  select(-cnt)
region_distances <- as_tibble(cbind(
  select(region_vectors, region),
  as.matrix(dist(select(region_vectors, -region), diag = FALSE))
))
region_distances[upper.tri(region_distances, diag = FALSE)] <- NA 
colnames(region_distances) <- c("region", pull(select(region_vectors, region)))
region_distances <- region_distances %>% 
  pivot_longer(-region, names_to = "region_2", values_to = "dist") %>% 
  left_join(select(er, region, economic_region)) %>% 
  left_join(select(
    er, 
    region_2 = region, 
    economic_region_2 = economic_region
  )) %>% 
  drop_na(dist) %>% 
  mutate(within = economic_region == economic_region_2)
region_distances_stat <- region_distances %>% 
  group_by(economic_region, within) %>% 
  summarise(dist = median(dist))
region_distances %>% 
  ggplot(aes(x = economic_region, y = dist, color = within)) +
  geom_violin(draw_quantiles = c(.5)) +
  geom_jitter(width = .1, size = .2)
arrange(region_distances, -dist)
region_distances %>% 
  ggplot(aes(x = dist)) +
  geom_freqpoly()

# Look at distribution by various classes
count(clustered, tag, sort = TRUE)
count(clustered, group, sort = TRUE)
count(clustered, strategy, sort = TRUE)
count(clustered, western, sort = TRUE)

count(clustered, region, tag) %>% 
  group_by(tag) %>% 
  summarise(prop = n / sum(n), region = region) %>% 
  filter(tag != "Misc") %>% 
  ungroup() %>%
  complete(region, tag) %>% 
  right_join(tiles, by = c("region" = "name")) %>% 
  #drop_na() %>% 
  ggplot(aes(x = col, y = -row, fill = prop)) +
  geom_raster(na.rm = FALSE) +
  scale_fill_binned(transform = "log10") +
  facet_wrap(vars(tag), ncol = 5) +
  coord_fixed()

count(clustered, region, western) %>% 
  group_by(region) %>% 
  summarise(prop = n / sum(n), western = western) %>% 
  filter(western == 1) %>%
  right_join(tiles, by = c("region" = "name")) %>% 
  ggplot(aes(x = col, y = -row, fill = prop)) +
  geom_raster(na.rm = FALSE) +
  geom_text(aes(label = code_en), color = "grey80") +
  scale_fill_binned() +
  coord_fixed()

count(clustered, region, western) %>% 
  group_by(region) %>% 
  summarise(prop = n / sum(n), western = western) %>% 
  filter(western == 1) %>% 
  arrange(-prop)

tail(count(clustered, region, sort = TRUE), 10)
clustered[clustered$region == "Чеченская республика", "name"]
