library(dbscan)
library(dplyr)
library(ggplot2)
library(glue)
library(here)
library(igraph)
library(RColorBrewer)
library(readr)
library(readxl)
library(rjson)
library(rulexicon)
library(sf)
library(spdep)
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
regions <- st_read(here("common", "regions.geojson")) %>% 
  left_join(
    read_csv(here("common", "regions.csv")),
    by = c("shapeISO" = "iso_code")
  ) %>% 
  arrange(code) %>% 
  select(code, name)
municipal_boundaries <- st_read(here("ru-mun-gadm.geojson"))
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
  cluster = dbscan_res$cluster,
  vectors_2d
)
count(clustered_names, cluster, sort = TRUE)

## Visualize cluster centers with numbers
clusters_plot <- clustered_names %>% 
  group_by(cluster) %>% 
  summarise(pc1 = mean(pc1), pc2 = mean(pc2)) %>% 
  ggplot(aes(
    x = pc1, y = pc2,
    label = str_pad(cluster, 2, "right")
  )) +
  geom_point(
    data = clustered_names, 
    aes(x = pc1, y = pc2),
    size = .5,
  ) +
  geom_text(hjust = -.5) +
  labs(x = "Component # 1", y = "Component #2") +
  theme_minimal()

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
cluster_labels <- read_csv(here("labels.csv"))
firms_lifetime <- count(firms, tin, name = "lifetime")
clustered <- clustered_names %>%
  left_join(cluster_labels) %>%
  right_join(firms) %>%
  right_join(firms_lifetime) %>%
  filter(name != "", lifetime >= 3) %>%
  select(
    name_id, tin, name, region, settlement, settlement_type,
    lat, lon, year,
    cluster, strategy) %>%
  drop_na(name, tin, region, settlement, lat, lon, year) %>%
  distinct(name, tin, .keep_all = TRUE)

# Table with clusters info
clusters_info_table <- count(clustered_names, cluster) %>% 
  left_join(cluster_labels)

cluster_labels %>% filter(strategy == "Uniqueness")

strategy_counts <- count(clusters_info_table, strategy, wt = n, sort = TRUE) %>% 
  mutate(share = 100 * n / sum(n))

count(clustered, region, strategy) %>% 
  complete(region, strategy, fill = list(n = 0)) %>% 
  group_by(region) %>% 
  summarise(
    x = (nth(n, 2) - nth(n, 3)) / sum(nth(n, 2), nth(n, 3)),
    y = (last(n) - first(n)) / sum(last(n), first(n))) %>% 
  arrange(-y) %>% print(n = 30)
  ggplot(aes(x = x, y = y)) +
  geom_point()

nodes <- tibble(
  label = unique(c(cluster_labels$cluster, cluster_labels$category, cluster_labels$strategy))
) %>% 
  mutate(id = row_number())
nodes <- create_node_df(n = 77, label = unique(c(cluster_labels$cluster, cluster_labels$category, cluster_labels$strategy)))
df <- cluster_labels %>% 
  left_join(nodes, by = c("category" = "label")) %>% 
  left_join(nodes, by = c("strategy" = "label")) 
edges1 <- create_edge_df(from = df$id.x, to = df$id.y)
edges2 <- create_edge_df(from = df$cluster, to = df$id.x)
g <- create_graph(nodes_df = nodes, edges = combine_edfs(edges1, edges2)) %>% 
  add_global_graph_attrs("rankdir", "LR", "graph")
get_global_graph_attr_info(g)
render_graph(g)

# Spatial autocorrelation
regions_w <- poly2nb(regions) %>% 
  addlinks1(39, c(32, 47, 60, 67, 78)) %>% # Kaliningrad
  addlinks1(65, c(25, 27, 41, 49)) %>% # Sakhalin
  nb2listw()

clusters_share_by_regions <- clustered %>% 
  count(region, cluster) %>% 
  complete(region, cluster, fill = list(n = 0)) %>% 
  group_by(region) %>% 
  mutate(share = n / sum(n)) %>%
  right_join(st_drop_geometry(regions), by = c("region" = "name")) %>% 
  arrange(code)

moran_test_by_regions <- data.frame(t(sapply(1:52, function(x) {
  mt <- clusters_share_by_regions %>% 
    filter(cluster == x) %>% 
    pull(share) %>% 
    moran.test(regions_w)
  unname(c(x, mt$estimate[1], mt$p.value))
}))) %>%
  rename(cluster = 1, moran_i = 2, p = 3) %>% 
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    signif = p_adj < .05
  ) %>%
  arrange(-signif, p_adj)

er_w <- er %>% 
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

moran_test_by_er <- data.frame(t(sapply(1:52, function(x) {
  mt <- clusters_share_by_regions %>% 
    filter(cluster == x) %>% 
    pull(share) %>% 
    moran.test(er_w)
  unname(c(x, mt$estimate[1], mt$p.value))
}))) %>%
  rename(cluster = 1, moran_i = 2, p = 3) %>% 
  mutate(
    p_adj = p.adjust(p, method = "holm"),
    signif = p_adj < .05
  ) %>%
  arrange(-signif, p_adj)

mloc <- clusters_share_by_regions %>% 
  filter(cluster == 28) %>% 
  pull(share) %>% 
  localmoran(er_w) %>% 
  hotspot(Prname="Pr(z != E(Ii))", cutoff = 0.005, 
          droplevels=FALSE)

er_nb <- er %>% 
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
  dnearneigh(d1 = 0, d2 = .1, longlat = FALSE)
p.adjustSP(mloc[, 5], er_nb, method = "holm")

# Strategies count and share by municipality
mun <- clustered %>% 
  filter(strategy != "Others") %>% 
  drop_na(lat, lon) %>% 
  mutate(lon = if_else(lon < 0, -(180 - lon), lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(municipal_boundaries) %>% 
  st_drop_geometry() %>% 
  count(NAME_1, NAME_2, strategy) %>% 
  group_by(NAME_1, NAME_2) %>% 
  mutate(share = n / sum(n)) %>%
  filter(strategy != "Others") %>% 
  pivot_wider(id_cols = c("NAME_1", "NAME_2"), names_from = strategy, values_from = share, values_fill = 0) %>% 
  rename(l = Lawyers, h = Helpers, s = `Service providers`) %>% 
  right_join(municipal_boundaries) %>% 
  st_as_sf()

mun <- clustered %>% 
  filter(strategy != "Others") %>% 
  drop_na(lat, lon) %>% 
  mutate(lon = if_else(lon < 0, -(180 - lon), lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(municipal_boundaries) %>% 
  st_drop_geometry() %>% 
  count(NAME_1, NAME_2, strategy) %>% 
  group_by(NAME_1, NAME_2) %>% 
  slice_max(n) %>% 
  #select(-n, -Others) %>% 
  right_join(municipal_boundaries) %>% 
  st_as_sf()

mun_base <- clustered %>% 
  drop_na(lat, lon) %>% 
  mutate(lon = if_else(lon < 0, -(180 - lon), lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(municipal_boundaries) %>% 
  st_drop_geometry() %>% 
  count(NAME_1, NAME_2, cluster)
municipal_boundaries <- st_make_valid(municipal_boundaries)
sapply(1:52, function (x) {
  d <- mun_base %>% 
    group_by(NAME_1, NAME_2) %>% 
    mutate(s = n / sum(n)) %>% 
    filter(cluster == x) %>% 
    right_join(municipal_boundaries) %>% 
    st_as_sf() %>% 
    arrange(NAME_1, NAME_2) %>% 
    replace_na(list(s = 0))
  
  m <- poly2nb(d)
  
  w <- nb2listw(m, zero.policy = TRUE)
  
  mt <- d %>% 
    pull(s) %>% 
    moran.test(w, na.action = na.exclude)
  print(c(x, mt$estimate[1], mt$p.value))
})

mun %>% 
  ggplot(aes(fill = n)) +
  geom_sf() +
  coord_sf(crs = ru_crs)

reg <- clustered %>% 
  drop_na(lat, lon) %>% 
  mutate(lon = if_else(lon < 0, -(180 - lon), lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(ru) %>% 
  st_drop_geometry() %>% 
  count(shapeISO, strategy) %>% 
  group_by(shapeISO) %>% 
  mutate(share = n / sum(n)) %>%
  filter(strategy != "Others") %>% 
  pivot_wider(id_cols = shapeISO, names_from = strategy, values_from = share, values_fill = 0) %>% 
  rename(l = Lawyers, h = Helpers, s = `Service providers`) %>% 
  right_join(ru) %>% 
  st_as_sf() %>% 
  arrange(shapeISO)

reg %>% 
  ggplot(aes(fill = l)) +
  geom_sf() +
  coord_sf(crs = ru_crs)

reg_base <- clustered %>% 
  drop_na(lat, lon) %>% 
  mutate(lon = if_else(lon < 0, -(180 - lon), lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(ru) %>% 
  st_drop_geometry() %>% 
  count(shapeISO, cluster) %>% 
  group_by(shapeISO) %>% 
  mutate(share = n / sum(n))

sapply(1:52, function(x) {
  mt <- reg_base %>% 
    filter(cluster == x) %>% 
    right_join(ru) %>% 
    st_as_sf() %>% 
    arrange(shapeISO) %>% 
    pull(share) %>% 
    moran.test(reg_matrix_w, na.action = na.exclude)
  c(x, mt$estimate[1], mt$p.value)
})

reg_share <- reg_base %>% 
  ungroup() %>% 
  filter(cluster == 36) %>%
  filter(share > quantile(share, .05), share < quantile(share, .95)) %>% 
  right_join(ru) %>% 
  st_as_sf() %>% 
  filter(!(shapeISO %in% c("RU-SAK", "RU-KGD", "UA-43", "UA-40"))) %>% 
  arrange(shapeISO) %>% 
  drop_na(share)
w <- reg_share %>% 
  st_centroid() %>% 
  knearneigh(k = 6) %>% 
  knn2nb() %>% 
  nb2listw()
reg_share %>% 
  pull(share) %>% 
  moran.plot(w)

sapply(1:52, function(x) {
  reg_share <- reg_base %>% 
    ungroup() %>% 
    filter(cluster == x) %>%
    filter(share > quantile(share, .05), share < quantile(share, .95)) %>% 
    right_join(ru) %>% 
    st_as_sf() %>% 
    filter(!(shapeISO %in% c("RU-SAK", "RU-KGD", "UA-43", "UA-40"))) %>% 
    arrange(shapeISO) %>% 
    drop_na(share)
  w <- reg_share %>% 
    st_centroid() %>% 
    knearneigh(k = 6) %>% 
    knn2nb() %>% 
    nb2listw()
  mt <- reg_share %>% 
    pull(share) %>% 
    moran.test(w)
  print(c(x, mt$estimate[1], mt$p.value))
})


reg_base %>% 
  right_join(ru) %>% 
  st_as_sf() %>% 
  filter(cluster == 28) %>% 
  group_by(shapeISO, cluster) %>% 
  summarise(share = sum(share)) %>% 
  ggplot(aes(fill = share)) +
  geom_sf() +
  coord_sf(crs = ru_crs)

cities_base <- clustered %>% 
  drop_na(lat, lon) %>% 
  mutate(lon = if_else(lon < 0, -(180 - lon), lon)) %>%
  count(settlement, lat, lon, cluster) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  group_by(settlement) %>% 
  mutate(total = sum(n), share = n / total) %>% 
  filter(total > 10)

cities_matrix <- cities_base %>% knearneigh(k = 6) %>% knn2nb()

cities_matrix_w <- nb2listw(cities_matrix, zero.policy = TRUE)

sapply(1:52, function(x) {
  mt <- cities_base %>% 
    filter(cluster == x) %>% 
    right_join(select(cities_base, settlement) %>% st_drop_geometry()) %>% 
    pull(share) %>% 
    moran.test(cities_matrix_w, na.action = na.exclude)
  c(x, mt$estimate[1], mt$p.value)
})

clustered %>% 
  drop_na(lat, lon) %>% 
  mutate(lon = if_else(lon < 0, -(180 - lon), lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_join(ru) %>% 
  st_drop_geometry() %>% 
  count(shapeISO, strategy) %>% 
  group_by(shapeISO) %>% 
  mutate(share = n / sum(n)) %>%
  filter(strategy != "Others") %>% 
  pivot_wider(id_cols = shapeISO, names_from = strategy, values_from = share, values_fill = 0) %>% 
  rename(l = Lawyers, h = Helpers, s = `Service providers`) %>% 
  right_join(ru) %>% 
  st_as_sf() %>% 
  pull(s) %>% 
  localmoran(reg_matrix_w, zero.policy = TRUE, na.action = na.exclude)

reg %>% pull(l) %>% localmoran(reg_matrix_w, zero.policy = TRUE, na.action = na.exclude)
reg_base %>% 
  filter(cluster == 9) %>% 
  right_join(ru) %>% 
  st_as_sf() %>% 
  arrange(shapeISO) %>% 
  pull(share) %>% 
  localmoran(reg_matrix_w, zero.policy = TRUE, na.action = na.exclude)

# Spatial autocorrelation
mun_matrix <- poly2nb(arrange(mun, NAME_1, NAME_2))

mun_matrix_w <- nb2listw(mun_matrix, zero.policy = TRUE)

mt <- mun %>% 
  pull(l) %>% 
  moran.test(mun_matrix_w, na.action = na.exclude)
mt$estimate[1]

# Spatial autocorrelation
reg_matrix <- poly2nb(arrange(ru, shapeISO))

reg_matrix_w <- nb2listw(reg_matrix, zero.policy = TRUE)

reg %>% 
  pull(h) %>% 
  moran.test(reg_matrix_w, na.action = na.exclude)

reg_umap <- clustered %>% 
  count(region, cluster) %>% 
  group_by(region) %>% 
  mutate(s = n / sum(n)) %>% 
  pivot_wider(
    id_cols = region,
    names_from = cluster,
    names_prefix = "c_",
    values_from = s,
    values_fill = 0,
  ) %>% 
  ungroup() %>% 
  select(-region) %>% 
  umap(n_components = 2, random_state = 42)

reg_2d <- data.frame(reg_umap["layout"]) %>% 
  rename(pc1 = layout.1, pc2 = layout.2)

reg_2d %>% 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point()

hc <- clustered %>% 
  count(region, cluster) %>% 
  group_by(region) %>% 
  mutate(s = n / sum(n)) %>% 
  pivot_wider(
    id_cols = region,
    names_from = cluster,
    names_prefix = "c_",
    values_from = s,
    values_fill = 0,
  ) %>% 
  ungroup() %>% 
  select(-region) %>% 
  dist() %>% 
  hclust(method = "average")

plot(hc)

reg_umap <- clustered %>% 
  count(region, strategy) %>% 
  filter(strategy != "Others") %>% 
  group_by(region) %>% 
  mutate(s = n / sum(n)) %>% 
  pivot_wider(
    id_cols = region,
    names_from = strategy,
    values_from = s,
    values_fill = 0,
  ) %>% 
  ungroup() %>% 
  select(-region) %>% 
  umap(n_components = 2, random_state = 42)

reg_2d <- data.frame(reg_umap["layout"]) %>% 
  rename(pc1 = layout.1, pc2 = layout.2)

reg_2d %>% 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point()

hdbscan(reg_2d, minPts = 3)

settl_clusters <- clustered %>% 
  count(settlement, strategy) %>% 
  #filter(!(cluster %in% excluded_clusters)) %>% 
  filter(strategy != "Others") %>% 
  group_by(settlement) %>% 
  mutate(t = sum(n), s = n / t) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = settlement,
    names_from = strategy,
    #names_prefix = "c_",
    values_from = s,
    values_fill = 0,
  ) %>% 
  ungroup()

settl_umap <- settl_clusters %>% 
  select(-settlement) %>% 
  umap(n_components = 2, random_state = 42)

settl_2d <- data.frame(settl_umap["layout"]) %>% 
  rename(pc1 = layout.1, pc2 = layout.2)

settl_2d %>% 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point()

settl_dbscan_res <- dbscan(settl_2d, eps = 3)
clustered_settl <- data.frame(
  name = settl_clusters$settlement, 
  cluster = settl_dbscan_res$cluster
)
count(clustered_settl, cluster, sort = TRUE)

clustered_settl %>% filter(cluster == 4)

cities <- read_csv("cities.csv")
clustered %>% 
  filter(!(cluster %in% excluded_clusters)) %>% 
  count(settlement, cluster) %>% 
  group_by(settlement) %>% 
  mutate(s = n / sum(n)) %>% 
  left_join(select(cities, city, population), by = c("settlement" = "city")) %>% 
  drop_na(population) %>% 
  ggplot(aes(x = population, y = s)) +
  geom_point() +
  facet_wrap(~cluster, ncol = 6)

clustered %>% 
  filter(!(cluster %in% excluded_clusters)) %>% 
  mutate(city = settlement_type == "г") %>% 
  count(city, cluster) %>% 
  group_by(city) %>% 
  mutate(s = n / sum(n)) %>% 
  pivot_wider(id_cols = cluster, names_from = city, values_from = s) %>% 
  arrange(-`FALSE`)

clustered %>% 
  filter(!(cluster %in% excluded_clusters)) %>% 
  mutate(city = settlement_type == "г") %>% 
  count(city, strategy) %>% 
  group_by(strategy) %>% 
  mutate(s = n / sum(n)) %>% 
  pivot_wider(id_cols = strategy, names_from = city, values_from = s) %>% 
  arrange(-`FALSE`)

clustered %>% 
  count(region, strategy) %>% 
  filter(strategy != "Others") %>% 
  group_by(region) %>% 
  mutate(s = n / sum(n)) %>% 
  pivot_wider(
    id_cols = region,
    names_from = strategy,
    values_from = s,
    values_fill = 0,
  ) %>% 
  ungroup() %>% 
  select(-region) %>% 
  hdbscan(minPts = 10)

# Semantic distance between regions
region_vectors <- vectors %>%
  right_join(firms) %>%
  right_join(firms_lifetime) %>%
  filter(name != "", !(name %in% geonames), lifetime >= 3) %>%
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


# Semantic distance between regions
settl_vectors <- vectors %>%
  right_join(firms) %>%
  right_join(firms_lifetime) %>%
  filter(name != "", lifetime >= 3) %>%
  distinct(name, tin, .keep_all = TRUE) %>%
  drop_na(settlement) %>%
  group_by(settlement) %>%
  summarise(across(dim_0:dim_255, mean), cnt = n()) %>%
  filter(cnt > quantile(cnt, .05)) %>%
  select(-cnt)

# Geographic distance between regions
centroids <- firms %>% 
  drop_na(lat, lon) %>% 
  distinct(settlement, .keep_all = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  select(settlement)
distances <- st_distance(centroids)
units(distances) <- "km"
colnames(distances) <- centroids$settlement
geo_distances <- pivot_longer(
  cbind(settlement = centroids$settlement, as.data.frame(distances)),
  cols = -settlement,
  names_to = "settlement_2",
  values_to = "geo_distance"
)

settl_names_distances <- as_tibble(cbind(
  select(settl_vectors, settlement),
  as.matrix(dist(select(settl_vectors, -settlement), diag = FALSE))
))
settl_names_distances[upper.tri(settl_names_distances, diag = FALSE)] <- NA
colnames(settl_names_distances) <- c("settlement", pull(select(settl_names_distances, settlement)))

# Joint data on distances
settl_names_distances <- settl_names_distances %>%
  pivot_longer(-settlement, names_to = "settlement_2", values_to = "namedist") %>%
  drop_na(namedist) %>%
  left_join(geo_distances)

fit1 <- lm(namedist ~ geo_distance, settl_names_distances)
summary(fit1)
