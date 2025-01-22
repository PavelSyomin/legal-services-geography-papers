library(dplyr)
library(ggplot2)
library(glue)
library(dbscan)
library(lightgbm)
library(plotly)
library(readr)
library(rjson)
library(rulexicon)
library(spatstat)
library(stopwords)
library(stringr)
library(tidyr)
library(tidytext)

data <- read_csv("../../ru-smb-companies/legal/panel.csv")
tiles <- read_csv("russia-tiles.csv")
stopwords <- data.frame(
  word = c(
    stopwords("ru", "stopwords-iso"),
    c("общество", "ограниченный", "ответственность", "акционерный")
  )
)

# For the conference
names_2021 <- data %>% 
  filter(
    kind == 1,
    year == 2021) %>% 
  select(region, area, settlement, settlement_type, name = org_name) %>% 
  unnest_tokens(word, name, drop = FALSE) %>% 
  left_join(hash_lemmas_opencorpora, by = c("word" = "token")) %>% 
  select(-word, word = lemma) %>% 
  anti_join(stopwords) %>% 
  drop_na(region, word)

count(names_2021, word, sort = TRUE) %>% write_csv("words.csv")

names_2021 %>% 
  filter(word == "земельный") %>% 
  pull(name)

names_2021 %>% select(word) %>% distinct(word) %>% write_csv("tokens.csv", col_names = FALSE)
tvectors <- read_delim("token_vectors.csv", delim = " ") %>% select(1:101)
tokens_umap <- umap(tvectors %>% select(-1), n_components = 2, random_state = 42)
tvectors_pc <- data.frame(tokens_umap["layout"]) %>% rename(pc1 = layout.1, pc2 =layout.2)

## Cluster with dbscan
dbscan_res2 <- dbscan(vectors_pc2, eps = 1)
vectors_pc2$cluster <- dbscan_res2$cluster
clustered_names2 <- cbind(select(vectors, name), vectors_pc2)
count(clustered_names2, cluster, sort = TRUE)

tvectors_pc %>% 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point()

top_tokens_by_region <- names_2021 %>% 
  count(region, word) %>% 
  filter(n >= 5) %>% 
  group_by(region) %>% 
  arrange(-n) %>% 
  #slice_head(n = 10) %>% 
  ungroup()
  
tokens <- count(top_tokens_by_region, word, wt = n, sort = TRUE)
#write_csv(tokens, "top-10-tokens-by-region.csv")
token_groups <- read_csv("top-10-tokens-by-region.csv")

token_groups_by_regions <- top_tokens_by_region %>% 
  left_join(token_groups, by = "word") %>% 
  count(region, group, wt = n.x) %>% 
  group_by(region) %>% 
  mutate(share = n / sum(n)) %>% 
  ungroup()

token_groups_by_regions %>% 
  group_by(region) %>% 
  slice_max(share, with_ties = FALSE) %>% 
  ungroup() %>% 
  right_join(tiles, by = c("region" = "name")) %>% 
  ggplot(aes(x = col, y = -row)) +
  geom_tile(aes(fill = group, alpha = share)) +
  geom_text(aes(label = code), size = 3) +
  coord_equal()

token_groups_by_regions %>% 
  filter(group == "geo") %>%  
  right_join(tiles, by = c("region" = "name")) %>% 
  ggplot(aes(x = col, y = -row)) +
  geom_tile(aes(fill = group)) +
  geom_text(aes(label = code), size = 3) +
  coord_equal()

filter(top_tokens_by_region, word == "сибирский")

# For the paper
names <- data %>% 
  filter(
    kind == 1, # companies only
    year <= 2021, # 2016–2021
  ) %>% 
  distinct(tin, org_name, .keep_all = TRUE) %>% 
  select(region, settlement, name = org_name, lat, lon) %>% 
  mutate(
    name_id = row_number(),
    settlement = case_when(
      region == "Москва" ~ "Москва",
      region == "Санкт-Петербург" ~ "Санкт-Петербург",
      TRUE ~ settlement)
  ) %>% 
  unnest_tokens(word, name) %>% 
  anti_join(stopwords)

# Lemmatization
unique_words <- unique(names$word)
mystem_res <- fromJSON(system(
  "./mystem --format=json",
  intern = TRUE, 
  input = toString(unique_words)))
lemmas <- data.frame(
  word = sapply(
    mystem_res, 
    function(item) item$text[1]),
  lemma = sapply(
    mystem_res, 
    function(item) ifelse(
      length(item$analysis) > 0, 
      item$analysis[[1]]$lex[1],
      NA))) %>% 
  distinct(word, lemma)

names <- names %>% 
  left_join(lemmas) %>% 
  select(-word, word = lemma) %>% 
  anti_join(stopwords) %>% 
  drop_na(word)

# Vectorization (FastText)
unique_words <- unique(names$word)
fasttext_res <- system(
  "./fastText/fasttext print-word-vectors ./fastText/cc.ru.300.bin",
  intern = TRUE,
  input = unique_words
)
vectors <- data.frame(
  do.call(rbind, strsplit(vectors, " ", fixed = TRUE))) %>% 
  rename(word = X1) %>% 
  mutate(across(-word, as.numeric)) %>% 
  distinct(word, .keep_all = TRUE)

names2 <- names %>% 
  left_join(vectors) %>% 
  group_by(name_id) %>% 
  summarise(
    region = first(region),
    settlement = first(settlement),
    lat = first(lat),
    lon = first(lon),
    name = paste0(word, collapse = " "),
    across(X2:X301, mean)
  )

# PCA (one region)
pca_res <- prcomp(select(filter(names2, region == "Свердловская область"), X2:X301))
summary(pca_res)
plot(pca_res)
biplot(pca_res)

ggplot(pca_res$x, aes(x = PC1, y = PC2)) +
  geom_point()

# Interactive plot
p <- ggplot(names2, aes(x = PC1, y = PC2, label = name)) +
  geom_point() 
ggplotly(p)

# KMeans
kmeans(pca_res$x[, 1:50], 8)$size

# HDBScan
hdb_res <- hdbscan(select(filter(names2, region == "Свердловская область"), X2:X301), minPts = 10)
unique(hdb_res$cluster)
filter(names2, region == "Свердловская область") %>% 
  mutate(cl = hdb_res$cluster) %>% 
  select(name, cl) %>% 
  View()

# Trying to remove rare / thematic stopwords
count(names, word, sort = TRUE) %>% count(n, sort = TRUE) %>% 
  mutate(
    t = n * nn,
    sh1 = nn / sum(nn),
    sh2 = t / sum(t),
    c1 = cumsum(sh1),
    c2 = cumsum(sh2)
    ) %>% 
  print(n = 25)

words_freq = count(names, word)
names3 <- names %>% 
  left_join(words_freq) %>% 
  filter(n > 10) %>% 
  left_join(vectors) %>% 
  group_by(name_id) %>% 
  summarise(
    region = first(region),
    settlement = first(settlement),
    lat = first(lat),
    lon = first(lon),
    name = paste0(word, collapse = " "),
    across(X2:X301, mean)
  )

pca_res3 <- prcomp(select(names3, X2:X301))
summary(pca_res3)
plot(pca_res3)
biplot(pca_res3)

ggplot(pca_res3$x, aes(x = PC1, y = PC2, col = PC3)) +
  geom_point()

# By regions
regions <- names %>% 
  left_join(vectors) %>% 
  group_by(region) %>% 
  summarise(
    across(X2:X301, mean)
  )
pca_res_reg <- prcomp(select(regions, X2:X301))
summary(pca_res_reg)

p <- regions %>% 
  bind_cols(pca_res_reg$x[, 1:2]) %>% 
  ggplot(aes(x = PC1, y = PC2, label = region)) +
  geom_point() +
  geom_text()
ggplotly(p)

# Remove also stopwords
words_freq = count(names, word, sort = TRUE) %>%
  mutate(t = cumsum(n), p = t / sum(n))
names5 <- names %>% 
  left_join(words_freq) %>% 
  filter(n > 3, n < 300) %>% 
  left_join(vectors) %>% 
  group_by(name_id) %>% 
  summarise(
    region = first(region),
    settlement = first(settlement),
    lat = first(lat),
    lon = first(lon),
    name = paste0(word, collapse = " "),
    across(X2:X301, mean)
  )
pca_res5 <- prcomp(select(names5, X2:X301))
summary(pca_res5)
plot(pca_res5)

ggplot(pca_res5$x, aes(x = PC1, y = PC2, col = PC3)) +
  geom_point()

regions5 <- names5 %>% 
  group_by(region) %>% 
  summarise(
    across(X2:X301, mean)
  )
pca_res_reg5 <- prcomp(select(regions5, X2:X301))
summary(pca_res_reg5)

p <- regions5 %>% 
  bind_cols(pca_res_reg5$x[, 1:2]) %>% 
  ggplot(aes(x = PC1, y = PC2, label = region)) +
  geom_point() +
  geom_text()
ggplotly(p)

# Check if idf stopwords differ from freq-based
words_idf <- names %>% 
  group_by(word, name_id) %>% 
  summarise(n = n()) %>% 
  count(word)
ndoc <- length(unique(names$name_id))
words_idf$idf = log(ndoc / words_idf$n)


# FastText vs YandexGPT vectors
# FastText
names_sample <- data %>% 
  filter(
    kind == 1, # companies only
    year <= 2021, # 2016–2021
  ) %>% 
  distinct(tin, org_name, .keep_all = TRUE) %>% 
  select(region, settlement, name = org_name, lat, lon) %>% 
  sample_n(1000) %>% 
  mutate(
    name_id = row_number(),
    settlement = case_when(
      region == "Москва" ~ "Москва",
      region == "Санкт-Петербург" ~ "Санкт-Петербург",
      TRUE ~ settlement)
  )
write_csv(names_sample[, "name"], "names_sample_geo_markup.csv")

names_sample_fs <- names_sample %>% 
  unnest_tokens(word, name) %>% 
  anti_join(stopwords) %>% 
  left_join(lemmas) %>% 
  select(-word, word = lemma) %>% 
  anti_join(stopwords) %>% 
  drop_na(word) %>% 
  left_join(vectors) %>% 
  group_by(name_id) %>% 
  summarise(
    region = first(region),
    settlement = first(settlement),
    lat = first(lat),
    lon = first(lon),
    name = paste0(word, collapse = " "),
    across(X2:X301, mean)
  )

names_sample_fs_pca <- prcomp(select(names_sample_fs, X2:X301))
#summary(names_fs_sample_pca)
plot(names_sample_fs_pca)

ggplot(names_sample_fs_pca$x, aes(x = PC1, y = PC2)) +
  geom_point()

names_sample_fs_clusters <- names_sample_fs %>% 
  mutate(
    cluster = kmeans(select(names_sample_fs, X2:X301), 10)$cluster
  ) %>% 
  select(name, cluster)

# FastText with sentence vectorization
names_sample_fs_sentences <- names_sample %>% 
  mutate(
    name = str_to_lower(name),
    name = str_remove(name, fixed("общество с ограниченной ответственностью")),
    name = str_remove(name, fixed("акционерное общество")),
    name = str_remove(name, fixed("закрытое акционерное общество")),
    name = str_remove_all(name, "[:punct:]"),
    name = str_trim(name)
  )

fasttext_sentences_res <- system(
  "./fastText/fasttext print-sentence-vectors ./fastText/cc.ru.300.bin",
  intern = TRUE,
  input = names_sample_fs_sentences$name
)
sentence_vectors <- data.frame(
  do.call(rbind, strsplit(fasttext_sentences_res, " ", fixed = TRUE))) %>% 
  mutate(across(everything(), as.numeric))

names_sample_fs_sentences <- names_sample_fs_sentences %>% 
  bind_cols(sentence_vectors)

names_sample_fs_sentences_pca <- prcomp(select(names_sample_fs_sentences, X1:X300))
summary(names_sample_fs_sentences_pca)
plot(names_sample_fs_sentences_pca)

ggplot(names_sample_fs_sentences_pca$x, aes(x = PC1, y = PC2)) +
  geom_point()

names_sample_fs_sentences_clusters <- names_sample_fs_sentences %>% 
  mutate(
    cluster = kmeans(select(names_sample_fs_sentences, X1:X300), 10)$cluster
  ) %>% 
  select(region, name, cluster)

# YandexGPT
names_sample_ygpt <- names_sample %>% 
  mutate(
    name = str_to_lower(name),
    name = str_remove(name, fixed("общество с ограниченной ответственностью")),
    name = str_remove(name, fixed("акционерное общество")),
    name = str_remove(name, fixed("закрытое акционерное общество")),
    name = str_remove_all(name, "[:punct:]"),
    name = str_trim(name)
  )

names_sample_ygpt[, c("name", "name_id")] %>% write_csv("names_sample_ygpt.csv")

names_sample_ygpt_online_vectors <- read_csv("names_sample_ygpt_vectors.csv")
names_sample_ygpt <- names_sample_ygpt %>% 
  left_join(names_sample_ygpt_online_vectors, by = "name_id")

names_sample_ygpt_pca <- prcomp(select(names_sample_ygpt, d0:d255))
summary(names_sample_ygpt_pca)
plot(names_sample_ygpt_pca)

ggplot(names_sample_ygpt_pca$x, aes(x = PC1, y = PC2)) +
  geom_point()

names_sample_ygpt_clusters <- names_sample_ygpt %>% 
  mutate(
    cluster = kmeans(select(names_sample_ygpt, d0:d255), 50)$cluster
  ) %>% 
  select(region, name, cluster)
names_sample_ygpt_clusters_by_regions <- count(names_sample_ygpt_clusters, region, cluster) %>% 
  arrange(cluster, -n) %>% 
  group_by(cluster) %>% 
  slice_head(n = 5)

kmeans(select(names_sample_ygpt, d0:d255), 50)$size

names_sample_ygpt_regions <- names_sample_ygpt %>% 
  group_by(region) %>% 
  summarise(across(d0:d255, mean))
names_sample_ygpt_regions_pca <- prcomp(select(names_sample_ygpt_regions, d0:d255))
summary(names_sample_ygpt_regions_pca)
plot(names_sample_ygpt_regions_pca)
ggplot(names_sample_ygpt_regions_pca$x, aes(x = PC1, y = PC2)) +
  geom_point()


# Word vectors
top1000w <- count(names, word, sort = TRUE) %>% 
  left_join(vectors) %>% 
  slice_head(n = 1000)

top1000w_pca <- top1000w %>% 
  select(contains("X")) %>% 
  prcomp()
plot(top1000w_pca)

p <- top1000w %>% 
  select(word, n) %>% 
  bind_cols(top1000w_pca$x[, 1:2]) %>% 
  slice_head(n = 100) %>% 
  ggplot(aes(x = PC1, y = PC2, size = n, label = word)) +
  geom_point(shape = 21, alpha = .3) +
  scale_size_continuous(transform = "log10")
ggplotly(p)

count(names, word, sort = TRUE) %>% 
  mutate(c = cumsum(n), s = c / sum(n)) %>% 
  filter(s < .7) %>% 
  tail()

kmeans(select(top1000w, -word, -n), 10)$size

# The same with YandexGPT
top1000w_ygpt <- count(names, word, sort = TRUE) %>% 
  mutate(word_id = row_number()) %>% 
  slice_head(n = 1000)
write_csv(select(top1000w_ygpt, word, word_id), "words_sample_ygpt.csv")

top1000w_ygpt_online_vectors <- read_csv("words_sample_ygpt_vectors.csv") %>% rename(word_id = name_id)
top1000w_ygpt_pca <- top1000w_ygpt %>% 
  left_join(top1000w_ygpt_online_vectors) %>% 
  select(starts_with("d")) %>% 
  prcomp()
plot(top1000w_ygpt_pca)
summary(top1000w_ygpt_pca)

p <- top1000w_ygpt %>% 
  select(word, n) %>% 
  bind_cols(top1000w_ygpt_pca$x[, 1:2]) %>% 
  slice_head(n = 100) %>% 
  ggplot(aes(x = PC1, y = PC2, size = n, label = word)) +
  geom_point(shape = 21, alpha = .3) +
  scale_size_continuous(transform = "log10")
ggplotly(p)

kmeans(top1000w_ygpt %>% 
         left_join(top1000w_ygpt_online_vectors) %>% 
         select(starts_with("d")), 10)$size

# Classifier geo/nogeo
names_geo <- read_csv("names_sample_geo_markup.csv")
count(names_geo, is_geo)

train_data <- names_sample_ygpt %>% 
  bind_cols(names_geo) %>% 
  select(name_id, d0:d255, is_geo) %>% 
  dplyr::slice(1:600)
eval_data <- names_sample_ygpt %>% 
  bind_cols(names_geo) %>%  
  select(name_id, d0:d255, is_geo) %>% 
  dplyr::slice(601:800)
test_data <- names_sample_ygpt %>% 
  bind_cols(names_geo) %>%  
  select(name_id, d0:d255, is_geo) %>% 
  dplyr::slice(801:1000)

train <- lgb.Dataset(
  data = as.matrix(select(train_data, -is_geo)),
  label = as.character(train_data$is_geo)
)
eval <- lgb.Dataset(
  data = as.matrix(select(eval_data, -is_geo)),
  label = as.character(eval_data$is_geo)
)
test <- lgb.Dataset(
  data = as.matrix(select(test_data, -is_geo)),
  label = as.character(test_data$is_geo)
)

fit <- lgb.train(
  params = list(
    objective = "binary"
  ),
  data = train,
  nrounds = 100,
  verbose = -1,
  valids = list(eval = eval)
)

predict(fit, as.matrix(select(test_data, -is_geo)), type = "class")

ppp_data <- firm_names_clusters %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(n, crs = ru_crs) %>% 
  filter(year == 2021) #, region != "Москва", region != "Санкт-Петербург")

firm_names_ppp <- as.ppp(
  ppp_data$geometry, W = as.owin(st_transform(ru, ru_crs)))
firm_names_ppp$marks <- ppp_data$group
firm_names_spp <- split(firm_names_ppp, marks(firm_names_ppp))
img <- density(firm_names_spp$Form, sigma = 125000)
png(filename = "density/img.png", width = 1440, height = 720)

firm_names_ppp_clusters <- as.ppp(
  ppp_data$geometry, W = as.owin(st_transform(ru, ru_crs)))
firm_names_ppp_clusters$marks <- ppp_data$cluster
firm_names_spp_clusters <- split(firm_names_ppp_clusters, marks(firm_names_ppp_clusters))
plot(density(firm_names_spp_clusters, sigma = 125000))

firm_names_ppp_tags <- as.ppp(
  ppp_data$geometry, W = as.owin(st_transform(ru, ru_crs)))
firm_names_ppp_tags$marks <- factor(ppp_data$tags)
firm_names_spp_tags <- split(firm_names_ppp_tags, marks(firm_names_ppp_tags))
dir.create("density")
mapply( 
  function (x, n) {
    img <- density(firm_names_spp_tags[[x]], sigma = 125000)
    png(filename = paste0("density/", n, ".png"), width = 1440, height = 720)
    plot(img, main = x)
    dev.off()
  },
  unique(ppp_data$tags),
  1:length(unique(ppp_data$tags))
)

ppp_data_all <- firm_names_clusters %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(n, crs = ru_crs) %>% 
  filter(year == 2021)
firm_names_ppp_all_tags <- as.ppp(
  ppp_data_all$geometry, W = as.owin(st_transform(ru, ru_crs)))
firm_names_ppp_all_tags$marks <- factor(ppp_data_all$tags)
firm_names_spp_all_tags <- split(firm_names_ppp_all_tags, marks(firm_names_ppp_all_tags))
dir.create("density_all")
mapply( 
  function (x, n) {
    img <- density(firm_names_spp_all_tags[[x]], sigma = 125000)
    png(filename = paste0("density_all/", n, ".png"), width = 1440, height = 720)
    plot(img, main = x)
    dev.off()
  },
  unique(ppp_data_all$tags),
  1:length(unique(ppp_data_all$tags))
)

firm_names_ppp_strategies <- as.ppp(
  ppp_data$geometry, W = as.owin(st_transform(ru, ru_crs)))
firm_names_ppp_strategies$marks <- factor(ppp_data$strategy)
firm_names_spp_strategies <- split(firm_names_ppp_strategies, marks(firm_names_ppp_strategies))
plot(density(firm_names_spp_strategies, sigma = 125000))

firm_names_ppp_mod <- as.ppp(
  ppp_data$geometry, W = as.owin(st_transform(ru, ru_crs)))
firm_names_ppp_mod$marks <- factor(ppp_data$innovation)
firm_names_spp_mod <- split(firm_names_ppp_mod, marks(firm_names_ppp_mod))
plot(density(firm_names_spp_mod, sigma = 125000))

firm_names_ppp_sec <- as.ppp(
  ppp_data$geometry, W = as.owin(st_transform(ru, ru_crs)))
firm_names_ppp_sec$marks <- factor(ppp_data$securitization)
firm_names_spp_sec <- split(firm_names_ppp_sec, marks(firm_names_ppp_sec))
plot(density(firm_names_spp_sec, sigma = 125000))

firm_names_ppp_ivd <- as.ppp(
  ppp_data$geometry, W = as.owin(st_transform(ru, ru_crs)))
firm_names_ppp_ivd$marks <- factor(ppp_data$individual)
firm_names_spp_ivd <- split(firm_names_ppp_ivd, marks(firm_names_ppp_ivd))
plot(density(firm_names_spp_ivd, sigma = 125000))





excluded_clusters <- c(
  1, 2, 3, 5, 14, 15, 18, 21, 24, 26, 27, 30, 34, 35, 39,
  42, 45, 46, 47, 49, 51 
)

tokenized_names <- clustered_names %>% 
  filter(!(cluster %in% excluded_clusters)) %>% 
  unnest_tokens(word, name, drop = FALSE)

tokens <- unique(tokenized_names$word)
mystem_res <- fromJSON(system(
  "./mystem --format=json",
  intern = TRUE, 
  input = toString(tokens)))
lemmas <- data.frame(
  word = sapply(
    mystem_res, 
    function(item) item$text[1]),
  lemma = sapply(
    mystem_res, 
    function(item) ifelse(
      length(item$analysis) > 0, 
      item$analysis[[1]]$lex[1],
      NA))) %>% 
  distinct(word, lemma)

tokenized_names <- tokenized_names %>% 
  left_join(lemmas) %>% 
  anti_join(stopwords, by = c("lemma" = "word"))

tokenized_names %>%
  select(name = lemma) %>%
  distinct() %>% 
  drop_na() %>% 
  write_csv("lemmas.csv")

lemmas_vectors <- read_csv("common/lemmas-vectors.csv")
lemmas_vectors_umap_res <- lemmas_vectors %>% 
  select(-name) %>% 
  umap(n_components = 2, random_state = 42)
lemmas_vectors_2d <- data.frame(lemmas_vectors_umap_res["layout"]) %>% 
  rename(pc1 = layout.1, pc2 =layout.2)
lemmas_vectors_2d %>% 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point()
lemmas_hdbscan_res <- hdbscan(lemmas_vectors_2d, minPts = 10)
lemmas_clusters <- cbind(
  lemmas_vectors %>% select(name),
  lemma_cluster = lemmas_hdbscan_res$cluster
)
count(lemmas_clusters, lemma_cluster, sort = TRUE)

filter(lemmas_clusters, lemma_cluster == 51)

lemmas_clusters %>% arrange(lemma_cluster) %>% write_csv("lemmas_clusters.csv")

tokenized_names %>% 
  left_join(lemmas_clusters, by = c("lemma" = "name")) %>% 
  filter(lemma_cluster == 13) %>% 
  distinct(name)

colSums(is.na.data.frame(tokenized_names))

vectors2 <- read_delim("vectors2.csv", delim = " ") %>% select(1:100)

set.seed(42)
## Dimensions reduction with umap
umap_res2 <- umap(vectors2, n_components = 2, random_state = 42)
vectors_pc2 <- data.frame(umap_res2["layout"]) %>% rename(pc1 = layout.1, pc2 =layout.2)

## Cluster with dbscan
dbscan_res2 <- dbscan(vectors_pc2, eps = 1)
vectors_pc2$cluster <- dbscan_res2$cluster
clustered_names2 <- cbind(select(vectors, name), vectors_pc2)
count(clustered_names2, cluster, sort = TRUE)

vectors_pc2 %>% 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point()

clustered_names2 %>% 
  group_by(cluster) %>% 
  summarise(pc1 = mean(pc1), pc2 = mean(pc2)) %>% 
  ggplot(aes(x = pc1, y = pc2, label = cluster)) +
  geom_text()

filter(clustered_names2, cluster == 5) %>% pull(name)

vectors3 <- read_delim("vectors3.csv", delim = " ") %>% select(1:100)

set.seed(42)
## Dimensions reduction with umap
umap_res3 <- umap(vectors3, n_components = 2, random_state = 42)
vectors_pc3 <- data.frame(umap_res3["layout"]) %>% rename(pc1 = layout.1, pc2 =layout.2)

## Cluster with dbscan
dbscan_res3 <- dbscan(vectors_pc3, eps = 1)
vectors_pc2$cluster <- dbscan_res2$cluster
clustered_names2 <- cbind(select(vectors, name), vectors_pc2)
count(clustered_names2, cluster, sort = TRUE)

vectors_pc3 %>% 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point()

clustered_names2 %>% 
  group_by(cluster) %>% 
  summarise(pc1 = mean(pc1), pc2 = mean(pc2)) %>% 
  ggplot(aes(x = pc1, y = pc2, label = cluster)) +
  geom_text()

filter(clustered_names2, cluster == 5) %>% pull(name)
