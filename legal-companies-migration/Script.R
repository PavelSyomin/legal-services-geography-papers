library(dplyr)
library(ggplot2)
library(glue)
library(readr)
library(tidyr)
library(sf)

# Load the data
data <- read_csv("../../ru-smb-companies/legal/smb.csv")
revexp <- read_csv("../../ru-smb-companies/legal/revexp.csv")
empl <- read_csv("../../ru-smb-companies/legal/empl.csv")
cities <- read_csv("cities.csv")
regions_geo <- st_read("ru.geojson")
regions <- read_csv("regions.csv")

# Number of unique companies
companies_count <- n_distinct(data[data$kind == 1, "tin"])

# Number of companies as of 2016-08-10 (to compare with Moiseeva2016)
companies_count_08_2016 <- nrow(filter(data, kind == 1, start_date == "2016-08-10"))

# Calculate the absolute numbers and shares of companies
# with changes in location
# Location is defined as change either in region or settlement
migration_count_distribution <- data %>% 
  filter(kind == 1) %>% 
  group_by(tin) %>%
  distinct(region, settlement) %>% 
  count() %>% 
  ungroup() %>% 
  count(n, name = "count") %>% 
  mutate(
    n = n - 1,
    share = 100 * count / sum(count))

# Draw it on plot
mig_count_distr_plot <- migration_count_distribution %>% 
  ggplot(aes(x = n, y = count)) +
  geom_col() +
  geom_text(
    aes(y = 1e6, label = scales::number(share, accuracy = 0.01, suffix = " %")),
    color = "gray30", size = 3
    ) + 
  geom_text(aes(label = count), vjust = -.5) + 
  scale_y_log10(limits = c(NA, 1e6), breaks = 10^(1:5), labels = ~ .x) +
  labs(
    x = "Number of migrations by a company", 
    y = "Number of companies (log10)") +
  theme_bw(base_size = 11, base_family = "Times New Roman") +
  theme(
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank())
mig_count_distr_plot

# Prepare the base dataset to analyze migrations of companies
max_empl <- c(15, 100, 250)
max_revenue <- c(120e6, 800e6, 2e9)
migrations <- data %>% 
  filter(kind == 1) %>% 
  group_by(tin) %>% 
  summarise(n = n_distinct(region, settlement)) %>%
  filter(n > 1) %>% 
  select(tin) %>% 
  left_join(data) %>% 
  group_by(tin) %>% 
  distinct(region, settlement, .keep_all = TRUE) %>% 
  arrange(start_date, end_date) %>% 
  mutate(
    next_region = lead(region),
    next_settlement = lead(settlement),
    next_lat = lead(lat),
    next_lon = lead(lon),
    date = lead(start_date),
    year = as.numeric(format(date, "%Y"))
  ) %>% 
  drop_na(next_region) %>% 
  ungroup() %>%
  left_join(revexp, by = c("tin", "year")) %>% 
  left_join(empl, by = c("tin", "year")) %>% 
  replace_na(list(revenue = 0, expenditure = 0, employees_count = 0)) %>% 
  mutate(
    employees_count = ifelse(employees_count > 500, 0, employees_count),
    revenue = ifelse(revenue > 1e10, 0, revenue),
    revenue = revenue / 1e6
  ) %>% 
  select(
    tin,
    region_from = region,
    settlement_from = settlement,
    lat_from = lat, 
    lon_from = lon,
    region_to = next_region, 
    settlement_to = next_settlement,
    lat_to = next_lat,
    lon_to = next_lon,
    date,
    category,
    revenue,
    empl = employees_count
  ) %>% 
  mutate(path = glue("LINESTRING ({lon_from} {lat_from}, {lon_to} {lat_to})")) %>%
  mutate(path = ifelse(grepl("NA", path), "LINESTRING EMPTY", path)) %>% 
  st_as_sf(wkt = "path", crs = "EPSG:4326") %>% 
  mutate(distance = as.numeric(st_length(path)) / 1000) %>% 
  mutate(distance = replace(distance, distance == 0, NA)) %>% 
  st_drop_geometry()

# Distribution of companies' sizes by migration status
smb_categories <- c("Микропредприятие", "Малое предприятие", "Среднее предприятие")
c1 <- data %>% 
  distinct(tin, .keep_all = TRUE) %>% 
  count(category)
c2 <- migrations %>% 
  distinct(tin, .keep_all = TRUE) %>% 
  count(category)
count_by_category <- inner_join(
  c1, c2, by = "category", suffix = c("_all", "_migrated")) %>% 
  mutate(category = smb_categories[category])
categories_test <- count_by_category %>% filter(category != "Среднее предприятие") %>% 
  mutate(non_migrated = n_all - n_migrated) %>% 
  select(mig = n_migrated, stay = non_migrated) %>% 
  fisher.test()

mig_by_category_plot <- count_by_category %>% 
  filter(category != "Среднее предприятие") %>% 
  mutate(
    non_migrated = n_all - n_migrated,
    share_migrated = n_migrated / n_all,
    share_non_migrated = non_migrated / n_all) %>%
  select(category, mig = share_migrated, stay = share_non_migrated) %>% 
  pivot_longer(cols = mig:stay, names_to = "option", values_to = "count") %>% 
  ggplot(aes(x = category, y = count, fill = option)) +
    geom_col(width = 0.5) +
    scale_x_discrete(labels = c("Микропредприятие"  = "Micro-business", "Малое предприятие" = "Small company")) +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_fill_discrete(name = "", labels = c("Migrated", "Stayed")) +
    coord_flip() +
    labs(x = "", y = "Share of companies") +
    theme_bw(base_size = 11, base_family = "Times New Roman") +
    theme(panel.grid.minor.x = element_blank())
    
# Distance of migrations
mig_distance_plot <- migrations %>% 
  drop_na(distance) %>% 
  ggplot(aes(x = distance)) +
  geom_freqpoly(bins = 50) +
  scale_x_continuous(trans = "log10", breaks = 10^(-1:3), labels = ~ .x) +
  theme_bw(base_size = 11, base_family = "Times New Roman") +
  labs(x = "Расстояние миграции, км (десятичный логарифм)",
       y = "Число фирм")

# Date of migrations
# Overall timeline
mig_timeline_plot <- migrations %>% 
  mutate(yearmon = as.Date(format(date, "%Y-%m-01"))) %>% 
  ggplot(aes(x = yearmon)) +
    geom_bar() +
    scale_x_date(
      date_breaks = "6 months", date_labels = "%m.%Y", 
      guide = guide_axis(angle = 90)) +
  labs(x = "Месяц и год", y = "Количество компаний") +
  theme_bw(base_size = 11, base_family = "Times New Roman") +
  theme(panel.grid.minor.y = element_blank())

# By month
mig_by_month_plot <- migrations %>% 
  mutate(month = format(date, "%m")) %>% 
  ggplot(aes(x = month)) +
  geom_bar() +
  labs(x = "Месяц", y = "Количество компаний") +
  theme_bw(base_size = 11, base_family = "Times New Roman") +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())

# Migration by regions
migration_by_region <- migrations %>% 
  select(region_from, region_to, revenue, empl) %>% 
  pivot_longer(region_from:region_to, names_to = "type", values_to = "region") %>% 
  drop_na(region) %>% 
  group_by(region, type) %>% 
  summarise(count = n(), revenue = sum(revenue), empl = sum(empl)) %>% 
  pivot_longer(count:empl, names_to = "var", values_to = "val") %>% 
  pivot_wider(c("region", "var"), names_from = type, values_from = val, values_fill = 0) %>% 
  mutate(change = region_to - region_from) %>% 
  select(region, var, change) %>% 
  pivot_wider(region, names_from = var, values_from = change) %>% 
  arrange(-count) %>% 
  left_join(select(regions, name, iso_code), by = c("region" = "name")) %>% 
  right_join(select(regions_geo, shapeISO), by = c("iso_code" = "shapeISO")) %>% 
  st_as_sf()

ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")
mig_by_region_plot <- migration_by_region %>% 
  ggplot() +
  geom_sf(aes(fill = count), size = .1) + 
  coord_sf(crs = ru_crs) +
  scale_fill_gradient2(name = "", low = "#d01c8b", high = "#4dac26") +
  theme_bw(base_size = 11, base_family = "Times New Roman")

migration_msk_spb <- migration_by_region %>% 
  filter(region %in% c("Москва", "Санкт-Петербург")) %>% 
  rename(settlement = region) %>% 
  select(-iso_code) %>% 
  st_drop_geometry()

# Migration by settlements
settl_coords <- rbind(
  select(distinct(data, settlement, .keep_all = TRUE), settlement, lat, lon),
  select(distinct(data, region, .keep_all = TRUE), settlement = region, lat, lon)
) %>% 
  drop_na(lat, lon) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")
migration_by_settlement <- migrations %>% 
  st_drop_geometry() %>% 
  select(settlement_from, settlement_to, revenue, empl) %>% 
  pivot_longer(settlement_from:settlement_to, names_to = "type", values_to = "settlement") %>% 
  drop_na(settlement) %>% 
  group_by(settlement, type) %>% 
  summarise(count = n(), revenue = sum(revenue), empl = sum(empl)) %>% 
  pivot_longer(count:empl, names_to = "var", values_to = "val") %>% 
  pivot_wider(c("settlement", "var"), names_from = type, values_from = val, values_fill = 0) %>% 
  mutate(change = settlement_to - settlement_from) %>% 
  select(settlement, var, change) %>% 
  pivot_wider(settlement, names_from = var, values_from = change) %>%
  arrange(-count) %>% 
  rbind(migration_msk_spb) %>% 
  left_join(settl_coords) %>% 
  st_as_sf()

migration_by_settlement_plot <- migration_by_settlement %>% 
  filter(abs(count) > 1) %>% 
  mutate(direction = count > 0, count = abs(count)) %>% 
  ggplot() +
  geom_sf(data = regions_geo, size = .5, fill = "white") +
  geom_sf(aes(color = direction), size = 1) +
  coord_sf(crs = ru_crs) +
  scale_color_manual(name = "", labels = c("Отток", "Приток"), values = c("gray50", "gray20")) +
  theme_bw(base_size = 11, base_family = "Times New Roman") +
  theme(legend.position = "bottom", legend.direction = "horizontal")
migration_by_settlement_plot

# Inter-regional vs intra-regional migrations
inter_intra <- migrations %>% 
  mutate(
    type = ifelse(
      region_from != region_to, 
      "inter-regional", 
      "intra-regional"),
    count = 1) %>% 
  group_by(type) %>% 
  summarise(across(c("count", "revenue", "empl"), sum, na.rm = TRUE))

# Inter-regional migration
# Raw paths (with separate direct/inverse variants)
regional_paths <- migrations %>% 
  filter(region_from != region_to) %>% 
  group_by(region_from, region_to) %>% 
  summarise(
    count = n(),
    revenue = sum(revenue),
    empl = sum(empl),
    .groups = "drop"
  ) %>%
  arrange(-count)
regional_paths

# With direct/inverse variants aggregated
regional_paths_combined <- regional_paths %>% 
  rowwise() %>% 
  mutate(
    path = glue_collapse(sort(c(region_from, region_to)), sep = " → "),
    direction = ifelse(
      all(sort(c(region_from, region_to)) == c(region_from, region_to)), 1, -1)) %>% 
  group_by(path) %>% 
  summarise(
    count = sum(count * direction),
    revenue = sum(revenue * direction),
    empl = sum(empl * direction)
  ) %>% 
  arrange(-abs(count)) %>% 
  rowwise() %>% 
  mutate(
    reverse = count >= 0,
    path = ifelse(
      reverse,
      path,
      glue_collapse(rev(strsplit(path, " → ", fixed = TRUE)[[1]]), sep = " → ")),
    count = ifelse(reverse, count, -count),
    revenue = ifelse(reverse, revenue, -revenue),
    empl = ifelse(reverse, empl, -empl)) %>% 
  select(-reverse)

regional_paths_combined_en <- regional_paths %>% 
  left_join(select(regions, name, iso_code_from = iso_code), by = c("region_from" = "name")) %>% 
  left_join(select(regions_geo, shapeISO, region_from_en = shapeName), by = c("iso_code_from" = "shapeISO")) %>% 
  left_join(select(regions, name, iso_code_to = iso_code), by = c("region_to" = "name")) %>% 
  left_join(select(regions_geo, shapeISO, region_to_en = shapeName), by = c("iso_code_to" = "shapeISO")) %>% 
  select(region_from = region_from_en, region_to = region_to_en,
         count, revenue, empl) %>% 
  rowwise() %>% 
  mutate(
    path = glue_collapse(sort(c(region_from, region_to)), sep = " → "),
    direction = ifelse(
      all(sort(c(region_from, region_to)) == c(region_from, region_to)), 1, -1)) %>% 
  group_by(path) %>% 
  summarise(
    count = sum(count * direction),
    revenue = sum(revenue * direction),
    empl = sum(empl * direction)
  ) %>% 
  arrange(-abs(count)) %>% 
  rowwise() %>% 
  mutate(
    reverse = count >= 0,
    path = ifelse(
      reverse,
      path,
      glue_collapse(rev(strsplit(path, " → ", fixed = TRUE)[[1]]), sep = " → ")),
    count = ifelse(reverse, count, -count),
    revenue = ifelse(reverse, revenue, -revenue),
    empl = ifelse(reverse, empl, -empl)) %>% 
  select(-reverse)

# Centralization/Decentralization
reg_central <- regional_paths %>% 
  mutate(type = case_when(
    region_from == "Санкт-Петербург" & region_to == "Ленинградская область" ~ "В центр",
    region_from == "Ленинградская область" & region_to == "Санкт-Петербург" ~ "Из центра",
    region_from == "Московская область" & region_to != "Москва" ~ "Из центра",
    region_to == "Московская область" & region_from != "Москва" ~ "В центр",
    region_from == "Москва" ~ "Из центра",
    region_to == "Москва" ~ "В центр",
    TRUE ~ "Прочее"
  )) %>% 
  group_by(type) %>% 
  summarise(across(count:empl, sum))

# Intra-regional migration
# Raw
settlement_paths <- migrations %>% 
  filter(region_from == region_to) %>% 
  group_by(settlement_from, settlement_to) %>%
  filter(settlement_from != settlement_to) %>%
  summarise(
    count = n(),
    revenue = sum(revenue),
    empl = sum(empl),
    .groups = "drop"
  ) %>%
  arrange(-count)
settlement_paths

# With direct/inverse paths aggregated
settlement_paths_combined <- settlement_paths %>% 
  rowwise() %>% 
  mutate(
    path = glue_collapse(sort(c(settlement_from, settlement_to)), sep = " → "),
    direction = ifelse(
      all(sort(c(settlement_from, settlement_to)) == c(settlement_from, settlement_to)), 1, -1)) %>% 
  group_by(path) %>% 
  summarise(
    count = sum(count * direction),
    revenue = sum(revenue * direction),
    empl = sum(empl * direction)
  ) %>% 
  arrange(-abs(count)) %>% 
  rowwise() %>% 
  mutate(path = ifelse(
    count >= 0,
    path,
    glue_collapse(rev(strsplit(path, " → ", fixed = TRUE)[[1]]), sep = " → ")),
    count = ifelse(count >= 0, count, -count))

# Centralization/decentralization
regional_capitals <- cities %>% 
  filter(capital_marker == 2) %>% 
  mutate(is_capital = TRUE) %>% 
  select(city, is_capital)
settl_central <- settlement_paths %>% 
  left_join(regional_capitals, by = c("settlement_from" = "city")) %>% 
  rename(from_capital = is_capital) %>% 
  left_join(regional_capitals, by = c("settlement_to" = "city")) %>% 
  rename(to_capital = is_capital) %>% 
  replace_na(list(from_capital = FALSE, to_capital = FALSE)) %>% 
  mutate(type = case_when(
    from_capital & !to_capital ~ "Из центра",
    to_capital & !from_capital ~ "В центр",
    TRUE ~ "Прочее"
  )) %>% 
  group_by(type) %>% 
  summarise(across(count:empl, sum))
