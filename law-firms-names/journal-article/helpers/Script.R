library(dplyr)
library(ggplot2)
library(here)
library(igraph)
library(ggraph)
library(knitr)
library(nanoparquet)
library(readr)
library(readxl)
library(sf)
library(spdep)
library(stopwords)
library(stringr)
library(tidyr)

sf_use_s2(FALSE) # disable to avoid errors in distances and intersections

# Load the data
data <- read_parquet(here("../datasets/law-firms/panel.parquet"))
er <- read_csv(here("journal-article/assets/economic-regions.csv"))
fd <- read_csv(here("journal-article/assets/federal-districts.csv"))
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
regions <- st_read(here("common/regions.geojson")) %>% 
  left_join(
    read_csv(here("journal-article/assets/regions.csv")),
    by = c("shapeISO" = "iso_code")
  ) %>% 
  arrange(code) %>% 
  select(code, name)
ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")
stopwords <- data.frame(
  word = c(
    stopwords("ru", "stopwords-iso"),
    c("общество", "ограниченный", "ответственность", "акционерный")
  )
)

# Prepare firms data
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
    settlement_type,
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

initial_firms_count <- firms %>% distinct(tin) %>% drop_na() %>% nrow()
unique_names <- distinct(firms, name)

# Save unique names to a file for processing with YandexGPT API
if (!get0("IS_PAPER", ifnotfound = FALSE)) {
  write_csv(
    unique_names,
    glue("names-{strftime(Sys.time(), '%Y-%m-%d-%H-%M-%S')}.csv"),
    col_names = FALSE,
  )
}

# Load vectors obtained from YandexGPT API
vectors <- read_parquet(here("common/names-vectors.parquet"))

# Cluster the names using vectors
## Find optimal number of clusters
if (!get0("IS_PAPER", ifnotfound = FALSE)) {
  n_clusters <- 2:100
  scores <- sapply(
    n_clusters,
    function(x) kmeans(
      slice_sample(select(vectors, dim_0:dim_255), n = 1000),
      centers = x)$tot.withins
  )
  ggplot(tibble(x = n_clusters, y = scores), aes(x = x, y = y)) +
    geom_line()
}

# Perform clustering and save a sample for manual analysis
set.seed(42)
fit <- kmeans(select(vectors, dim_0:dim_255), centers = 50)
clustered_names <- data.frame(
  name = vectors$name,
  cluster = fit$cluster
)
if (!get0("IS_PAPER", ifnotfound = FALSE)) {
  clustered_names %>%
    group_by(cluster) %>%
    slice_sample(n = 20) %>%
    select(name) %>%
    write_csv(
      glue("names-sample-{strftime(Sys.time(), '%Y-%m-%d-%H-%M-%S')}.csv")
    )
}

# Load the results of manual analysis and join them with names
cluster_labels <- read_excel(here("journal-article/assets/cluster-labels.xlsx"))
firms_lifetime <- count(firms, tin, name = "lifetime")
clustered <- clustered_names %>%
  left_join(cluster_labels) %>%
  right_join(firms) %>%
  right_join(firms_lifetime) %>%
  filter(name != "", lifetime >= 3) %>%
  select(
    name_id, tin, name, region, settlement,
    lat, lon, year, cluster, category, strategy
  ) %>%
  drop_na(name, tin, region, settlement, lat, lon, year) %>%
  distinct(name, tin, .keep_all = TRUE) %>% 
  mutate(category = factor(category), strategy = factor(strategy))

valid_firms_count <- nrow(clustered)

# Analyse the clusterisation results and visualise them
# Table with clusters info
clusters_info_table <- count(clustered_names, cluster) %>% 
  left_join(cluster_labels) %>% 
  select(cluster, n, description, category, strategy)

# Count by strategies
strategy_counts <- count(clusters_info_table, strategy, wt = n, sort = TRUE) %>% 
  mutate(share = 100 * n / sum(n))

# Dendrogram
parent_to_strategies <- clusters_info_table %>% 
  count(strategy, wt = n) %>% 
  mutate(from = "Names", cnt_label = n) %>% 
  select(from, to = strategy, n, cnt_label)
strategies_to_categories <- clusters_info_table %>% 
  count(category, strategy, wt = n) %>% 
  mutate(cnt_label = "") %>% 
  select(from = strategy, to = category, n, cnt_label)
g <- rbind(
  parent_to_strategies,
  strategies_to_categories
) %>% 
  graph_from_data_frame()

names_classification_plot <- ggraph(g, layout = "dendrogram") + 
  geom_edge_diagonal(
    aes(width = n, label = cnt_label),
    angle_calc = "along", label_dodge = unit(3, "mm")
  ) +
  geom_node_label(aes(label = name)) +
  scale_y_continuous(expand = expansion(mult = .25)) +
  scale_edge_width_continuous(
    name = "# of names",
    breaks = c(500, 1000, 2500, 5000, 10000), 
    range = c(.5, 2.5)
  ) +
  coord_flip() +
  theme_void() +
  theme(
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.title.position = "top"
  )
  

# Spatial autocorrelation
## Various neighbours identification techniques
## and corresponding weight matrices
### By geometry intersection
regions_nw <- poly2nb(regions) %>% 
  addlinks1(39, c(32, 47, 60, 67, 78)) %>% # Kaliningrad
  addlinks1(65, c(25, 27, 41, 49)) %>% # Sakhalin
  nb2listw()

### By 3, 4, 6 closest neighbours
regions_k3w <- st_centroid(regions) %>% 
  knearneigh(k = 3) %>% 
  knn2nb() %>% 
  nb2listw()

regions_k4w <- st_centroid(regions) %>% 
  knearneigh(k = 4) %>% 
  knn2nb() %>% 
  nb2listw()

regions_k6w <- st_centroid(regions) %>% 
  knearneigh(k = 6) %>% 
  knn2nb() %>% 
  nb2listw()

# By economic regions
regions_erw <- er %>% 
  left_join(
    st_drop_geometry(regions), 
    by = c("region" = "name") # to sort by codes
  ) %>% 
  mutate(
    x = as.numeric(factor(economic_region)),
    y = x # pseudo-coords to use out-of-the-box distance neighbours
  ) %>% 
  arrange(code) %>% 
  select(x, y) %>% 
  dnearneigh(d1 = 0, d2 = .1, longlat = FALSE) %>% # 0 within an economic region
  nb2listw()

# By federal districts
regions_fdw <- fd %>% 
  left_join(
    st_drop_geometry(regions), 
    by = c("region" = "name") # to sort by codes
  ) %>% 
  mutate(
    x = as.numeric(factor(federal_district)),
    y = x # pseudo-coords to use out-of-the-box distance neighbours
  ) %>% 
  arrange(code) %>% 
  select(x, y) %>% 
  dnearneigh(d1 = 0, d2 = .1, longlat = FALSE) %>% # 0 within an economic region
  nb2listw()

## Prepare the data for autocorrelation calculation
clusters_share_by_regions <- clustered %>% 
  count(region, category) %>% 
  complete(region, category, fill = list(n = 0)) %>% 
  group_by(region) %>% 
  mutate(share = n / sum(n)) %>%
  right_join(st_drop_geometry(regions), by = c("region" = "name")) %>% 
  arrange(code)

## Global Moran's I
category_ids <- 1:length(levels(clustered$category))
calc_global_moran_i <- function(category_id, weights_matrix, weights_type_id) {
  mt <- clusters_share_by_regions %>% 
    filter(as.numeric(category) == category_id) %>% 
    pull(share) %>% 
    moran.test(weights_matrix)
  unname(c(category_id, mt$estimate[1], mt$p.value, weights_type_id))
}

moran_test_nw <- data.frame(t(sapply(
  category_ids, calc_global_moran_i, regions_nw, 1
))) 

moran_test_k3w <- data.frame(t(sapply(
  category_ids, calc_global_moran_i, regions_k3w, 2
)))

moran_test_k4w <- data.frame(t(sapply(
  category_ids, calc_global_moran_i, regions_k4w, 3
)))

moran_test_k6w <- data.frame(t(sapply(
  category_ids, calc_global_moran_i, regions_k6w, 4
)))

moran_test_erw <- data.frame(t(sapply(
  category_ids, calc_global_moran_i, regions_erw, 5
)))

moran_test_fdw <- data.frame(t(sapply(
  category_ids, calc_global_moran_i, regions_fdw, 6
)))

moran_test_global_res <- rbind(
  moran_test_nw, moran_test_k3w, moran_test_k4w,
  moran_test_k6w, moran_test_erw, moran_test_fdw
) %>% 
  rename(category = 1, moran_i = 2, p = 3, weights_type = 4) %>% 
  mutate(
    weights_type = factor(
      weights_type, 
      1:6, 
      c("Common border", "3 nearest", "4 nearest",
        "6 nearest", "Economic region", "Federal district")
    )
  ) %>% 
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    signif = p_adj < .05,
    category = factor(category, labels = levels(clustered$category))
  ) %>%
  arrange(p_adj, weights_type) %>% 
  select(category, moran_i, p_adj, weights_type, signif)

moran_test_global_res_plot <- moran_test_global_res %>% 
  filter(signif) %>% 
  mutate(category = as.character(category)) %>% 
  complete(weights_type, category, fill = list(p_adj = 1)) %>% 
  mutate(
    p_adj_mark = cut(
      p_adj, 
      c(0, 1e-3, 1e-2, 5e-2, 1), 
      labels = c("p < 0.001", "p < 0.01", "p < 0.05", "insig.")
    )
  ) %>% 
  ggplot(aes(x = category, y = weights_type, fill = p_adj_mark)) +
  geom_raster() +
  geom_text(aes(label = round(moran_i, 1))) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_brewer(
    name = "Statistical\nsignificance"
  ) +
  coord_fixed() +
  labs(
    x = "Law firm naming pattern",
    y = "Type of weights matrix"
  ) +
  theme_minimal()
moran_test_global_res_plot

## Local Moran's I
categories_for_localmoran <- moran_test_global_res %>% 
  filter(signif) %>% 
  pull(category) %>% 
  as.numeric() %>% 
  sort() %>% 
  unique()

calc_local_moran_i <- function(category_id, weights_matrix, weights_martix_type) {
  clusters_share_by_regions %>% 
    filter(as.numeric(category) == category_id) %>% 
    pull(share) %>% 
    localmoran(weights_matrix) %>% 
    hotspot(
      Prname="Pr(z != E(Ii))",
      cutoff = 0.05, 
      p.adjust = "holm",
      droplevels=FALSE
    ) %>% 
    data.frame(
      category = category_id,
      group = ., 
      code = regions$code, 
      weights_type = weights_martix_type
    )
}

localmoran_nw <- do.call(rbind, lapply(
  categories_for_localmoran, calc_local_moran_i, regions_nw, 1
))

localmoran_k3w <- do.call(rbind, lapply(
  categories_for_localmoran, calc_local_moran_i, regions_k3w, 2
))

localmoran_k4w <- do.call(rbind, lapply(
  categories_for_localmoran, calc_local_moran_i, regions_k4w, 3
))

localmoran_k6w <- do.call(rbind, lapply(
  categories_for_localmoran, calc_local_moran_i, regions_k6w, 4
))

localmoran_erw <- do.call(rbind, lapply(
  categories_for_localmoran, calc_local_moran_i, regions_erw, 5
))

localmoran_fdw <- do.call(rbind, lapply(
  categories_for_localmoran, calc_local_moran_i, regions_fdw, 6
))

localmoran_res <- rbind(
  localmoran_nw, localmoran_k3w, localmoran_k4w,
  localmoran_k6w, localmoran_erw, localmoran_fdw
) %>% 
  filter(group != "<NA>") %>% 
  count(category, code, group) %>% 
  right_join(regions %>% select(code) %>% st_drop_geometry()) %>% 
  complete(category, code) %>% 
  drop_na(category) %>% 
  mutate(
    category = factor(
      category, 
      labels = paste0("(", letters[1:3], ")")
    )
  ) %>% 
  right_join(regions) %>% 
  st_as_sf()

localmoran_maps <- localmoran_res %>% 
  ggplot(aes(fill = group, alpha = n)) +
  geom_sf() +
  coord_sf(crs = ru_crs) +
  scale_alpha_continuous(
    name = "Confidence level"
  ) +
  scale_fill_discrete(
    name = "Region group",
    labels = c("Low-High", "High-High", "Insignificant"),
    na.value = "gray90"
  ) +
  facet_wrap(vars(category), ncol = 1) +
  theme_void() +
  theme(
    strip.text = element_text(hjust = 0)
  ) +
  labs(caption = "Boundaries source: www.geoboundaries.org (CC BY 4.0)")
