library(dbscan)
library(dplyr)
library(ggplot2)
library(glue)
library(here)
library(readr)
library(readxl)
library(rjson)
library(rulexicon)
library(sf)
library(stopwords)
library(stringr)
library(tidyr)
library(tidytext)
library(umap)
library(units)

sf_use_s2(FALSE) # disable to avoid errors in distances and intersections

# Load the data
data <- read_csv(here("common", "panel.csv"))
er <- read_csv(here("journal-article", "economic-regions.csv"))
er_short_labels <- tibble(
  economic_region = sort(unique(er$economic_region)),
  label = c("C", "CC", "ES", "FE", "N", "NC", "NW", "U", "V", "VV", "WS")
)
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
ru <- st_read(here("common", "ru.geojson"))
ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")
stopwords <- data.frame(
  word = c(
    stopwords("ru", "stopwords-iso"),
    c("общество", "ограниченный", "ответственность", "акционерный")
  )
)

# Preprocess the data
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
if (!get0("IS_PAPER", ifnotfound = FALSE)) {
  write_csv(
    unique_names,
    glue("names-{strftime(Sys.time(), '%Y-%m-%d-%H-%M-%S')}.csv"),
    col_names = FALSE,
    eol = ". "
  )
}

# Load vectors obtained from YandexGPT API
vectors <- read_csv(here("common", "names-vectors.csv"))

# Clustering
## Reduce to 2D with UMAP
vectors_umap_res <- vectors %>% 
  select(-name) %>% 
  umap(n_components = 2, random_state = 42)
vectors_2d <- data.frame(umap_res["layout"]) %>%
  rename(pc1 = layout.1, pc2 = layout.2)

## Cluster with dbscan
dbscan_res <- dbscan(vectors_2d, eps = 3)
clustered_names <- data.frame(
  name = vectors$name, 
  cluster = vectors_2d$cluster
)
count(clustered_names, cluster, sort = TRUE)

# Visualize clusters
vectors_2d %>% 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point()

# Visualize cluster centers with numbers
clustered_names %>% 
  group_by(cluster) %>% 
  summarise(pc1 = mean(pc1), pc2 = mean(pc2)) %>% 
  ggplot(aes(x = pc1, y = pc2, label = cluster)) +
  geom_text()

# Save a sample for manual analysis
if (!get0("IS_PAPER", ifnotfound = FALSE)) {
  clustered_names %>%
    group_by(cluster) %>%
    slice_sample(n = 20) %>%
    select(cluster, name) %>%
    write_csv(
      glue("names-sample-{strftime(Sys.time(), '%Y-%m-%d-%H-%M-%S')}.csv")
    )
}

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

# Semantic distance between regions
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

# Neighbors
neighboring_regions <- as_tibble(cbind(
  iso = ru$shapeISO,
  as.data.frame(st_intersects(ru, ru, sparse = FALSE, remove_self = TRUE))
))
colnames(neighboring_regions) <- c("iso", ru$shapeISO)
neighboring_regions <- pivot_longer(
  neighboring_regions,
  -iso,
  names_to = "iso_2",
  values_to = "is_neighbor"
)

# Geographic distance between regions
centroids <- st_centroid(ru) %>% select(iso = shapeISO)
distances <- st_distance(centroids)
units(distances) <- "km"
colnames(distances) <- centroids$iso
geo_distances <- pivot_longer(
  cbind(iso = centroids$iso, as.data.frame(distances)),
  cols = -iso,
  names_to = "iso_2",
  values_to = "geo_distance"
)

region_names_distances <- as_tibble(cbind(
  select(region_vectors, region),
  as.matrix(dist(select(region_vectors, -region), diag = FALSE))
))
region_names_distances[upper.tri(region_names_distances, diag = FALSE)] <- NA
colnames(region_names_distances) <- c("region", pull(select(region_names_distances, region)))

# Joint data on distances
region_names_distances <- region_names_distances %>%
  pivot_longer(-region, names_to = "region_2", values_to = "namedist") %>%
  left_join(er) %>%
  left_join(select(
    er,
    region_2 = region,
    economic_region_2 = economic_region,
    iso_code_2 = iso_code
  )) %>%
  drop_na(namedist) %>%
  mutate(within = economic_region == economic_region_2) %>%
  left_join(neighboring_regions, by = c("iso_code" = "iso", "iso_code_2" = "iso_2")) %>%
  left_join(geo_distances, by = c("iso_code" = "iso", "iso_code_2" = "iso_2"))

fit1 <- lm(namedist ~ within + is_neighbor + geo_distance, region_names_distances)
summary(fit1)

# Plot about neighborhood (Figure 1)
neighbors_plot <- region_names_distances %>%
  ggplot(aes(x = namedist, y = is_neighbor)) +
  geom_boxplot() +
  geom_text(
    aes(
      label = after_stat(paste("M == ", round(xmiddle, 2))),
      x = stage(namedist, after_stat = xmiddle)),
    stat = "boxplot",
    vjust = -.75,
    parse = TRUE,
    size = 3,
    family = "Segoe UI Semilight",
    angle = 90
  ) +
  scale_y_discrete(labels = c("Other\nregions", "Neighboring\nregions")) +
  labs(
    x = "Euclidean distance between region vectors",
    y = ""
  ) +
  theme_bw(base_family = "Segoe UI Semilight", base_size = 9)
neighbors_plot

if (!get0("IS_PAPER", ifnotfound = FALSE)) {
  t.test(namedist ~ is_neighbor, data = region_names_distances)
  wilcox.test(namedist ~ is_neighbor, data = region_names_distances)
  region_names_distances %>% group_by(is_neighbor) %>% summarise(m = median(namedist))
}

# Plot about distance (Figure 2)
distance_plot <- region_names_distances %>%
  ggplot(aes(x = geo_distance, y = namedist)) +
  geom_point(color = "grey50", size = 1, shape = 21) +
  geom_smooth(method = "lm", se = FALSE) +
  annotate(
    "label",
    x = as_units(Inf, "km"),
    y = Inf,
    label = paste(
      "r[Pearson] == ",
      round(cor.test(~namedist+geo_distance, data = region_names_distances)$estimate, 2)
    ),
    hjust = 1,
    vjust = 1,
    parse = TRUE,
    size = 3,
    family = "Segoe UI Semilight",
    label.size = 0,
  ) +
  labs(
    x = "Geographical distance between regions",
    y = "Distance between region vectors"
  ) +
  theme_bw(base_family = "Segoe UI Semilight", base_size = 9)
distance_plot

if (!get0("IS_PAPER", ifnotfound = FALSE)) {
  cor.test(~namedist+geo_distance, data = filter(region_names_distances))$estimate
}

# Plot about economic regions (Figure 3)
economic_regions_distances <- region_names_distances %>%
  ggplot(aes(x = within, y = namedist)) +
  geom_violin(draw_quantiles = c(.5)) +
  geom_text(
    aes(
      y = stage(namedist, after_stat = 0),
      label = after_stat(paste("M == ", round(middle, 2)))
    ),
    stat = "boxplot",
    vjust = -.25,
    parse = TRUE,
    size = 3,
    family = "Segoe UI Semilight",
  ) +
  scale_x_discrete(
    name = "Distance type",
    labels = c("Inside–outside", "Within")
  ) +
  facet_wrap(vars(economic_region), ncol = 3) +
  labs(
    y = "Semantic distance between regions"
  ) +
  theme_bw(base_family = "Segoe UI Semilight", base_size = 9)
economic_regions_distances

# Map of regions by naming strategy (Figure 4)
er_geo <- ru %>%
  left_join(er, by = c("shapeISO" = "iso_code")) %>%
  group_by(economic_region) %>%
  summarise() %>%
  left_join(er_short_labels)

naming_strategies <- clustered %>%
  filter(group != "Misc") %>%
  count(region, group, sort = TRUE) %>%
  group_by(region) %>%
  summarise(
    group = str_to_lower(group),
    share = round(100 * n / sum(n)),
    main_group = first(group),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c("region", "main_group"),
    names_from = group,
    values_from = share
  ) %>%
  mutate(
    diff = (service - law),
    type = case_when(
      diff < -3 ~ -1,
      diff > 3 ~ 1,
      is.na(diff) ~ NA_real_,
      TRUE ~ 0
    ),
    type = factor(
      type,
      labels = c("Law > Service", "Law ≈ Service", "Service > Law")
    )
  ) %>%
  right_join(er) %>%
  right_join(ru, by = c("iso_code" = "shapeISO")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(linewidth = .05, aes(fill = type)) +
  geom_sf(data = er_geo, linewidth = .5, fill = "transparent") +
  geom_sf_label(
    data = er_geo,
    aes(label = label),
    family = "Segoe UI Semilight",
    label.r = unit(0, "mm"),
    label.size = 0,
    label.padding = unit(0, "mm"),
    fill = "gray30",
    color = "gray90"
  ) +
  scale_fill_brewer(name = "Naming strategy", palette = "PRGn") +
  scale_discrete_identity(
    aesthetics = "label",
    name = "Economic region",
    breaks = er_geo$label,
    labels = er_geo$economic_region,
    guide = "legend"
  ) +
  coord_sf(crs = ru_crs, expand = FALSE) +
  theme_void(base_family = "Segoe UI Semilight", base_size = 9) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.justification = c(0.1, 1)
  )
naming_strategies




library(arrow)
rfsd2023 <- read_parquet("common/part-0.parquet", as_data_frame = FALSE)

rfsd2023df <- rfsd2023 %>% 
  filter(okved == "69.10" | okved == "69.1") %>% 
  select(inn, region, eligible, okved, okopf) %>% 
  collect() %>% 
  filter(substr(okopf, 1, 1) == "1")

match <- full_join(
  rfsd2023df, 
  filter(data, year == 2023, kind == 1) %>% distinct(tin, .keep_all = TRUE) %>% select(tin, region),
  by = c("inn" = "tin")
)
colSums(is.na.data.frame(match))
count(match, region.x, is.na(region.y)) %>% 
  group_by(region.x) %>% 
  summarise(share = last(n) / sum(n))


tvygpt <- read_csv("common/tokens-vectors.csv")
set.seed(42)
## Dimensions reduction with umap
tv_umap <- umap(tvygpt %>% select(-1), n_components = 2, random_state = 42)
tv_pc <- data.frame(tv_umap["layout"]) %>% rename(pc1 = layout.1, pc2 =layout.2)

## Cluster with dbscan
dbscan_tv <- dbscan(tv_pc, eps = .05)
tv_pc$cluster <- dbscan_tv$cluster
clustered_tokens <- cbind(select(tvygpt, name), tv_pc)
count(clustered_tokens, cluster, sort = TRUE)

tv_pc %>% 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point()

clustered_tokens %>% 
  ggplot(aes(x = pc1, y = pc2, col = factor(cluster))) +
  geom_point(show.legend = FALSE)

filter(clustered_tokens, cluster == 8) %>% pull(name)

clustered_tokens %>% arrange(cluster) %>% select(cluster, word = name) %>% group_by(cluster) %>% slice_head(n = 10) %>% write_csv("token_clusters.csv")

as.numeric(filter(tvygpt, name == "налоговый")[1, -1])

s <- apply(vectors[1:10000, -1], 1, function (x){
  A <- x[2:length(x)]
  B <- as.numeric(filter(tvygpt, name == "налоговый")[1, -1])
  sum(A*B)/sqrt(sum(A^2)*sum(B^2))
})

cbind(vectors[1:1000, 1], s) %>% arrange(-s) %>% slice_head(n = 10)

tv_more3 <- tvygpt %>% 
  left_join(count(names_2021, word, sort = TRUE), by = c("name" = "word")) %>% 
  filter(n > 3)
tv_more3_umap <- tv_more3 %>% 
  select(-name, -n) %>% 
  umap(n_components = 2, random_state = 42)
tv_more3_pc <- data.frame(tv_more3_umap["layout"]) %>% rename(pc1 = layout.1, pc2 =layout.2)
dbscan_tv_more3 <- dbscan(tv_more3_pc, eps = .1)
tv_more3_pc$cluster <- dbscan_tv_more3$cluster
clustered_tokens_more3 <- cbind(select(tv_more3, name), tv_more3_pc)
count(clustered_tokens_more3, cluster, sort = TRUE)
filter(clustered_tokens_more3, cluster == 7) %>% pull(name)

tv_more3_pc %>% 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point()
