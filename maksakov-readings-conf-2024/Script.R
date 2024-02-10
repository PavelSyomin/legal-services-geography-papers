library(dplyr)
library(ggplot2)
library(glue)
library(readr)
library(tidyr)
library(sf)
library(units)

# Load the data
data <- read_csv("../../ru-smb-companies/legal/smb.csv")
revexp <- read_csv("../../ru-smb-companies/legal/revexp.csv")
empl <- read_csv("../../ru-smb-companies/legal/empl.csv")
cities <- read_csv("cities.csv")
regions <- st_read("ru.geojson")

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
    x = "Count of migrations by a company", 
    y = "Count of companies (log10 scale)") +
  theme_bw() +
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
    revenue = ifelse(revenue > 1e10, 0, revenue)
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
    scale_y_continuous(labels = scales::label_percent()) +
    scale_fill_discrete(name = "", labels = c("Migrated", "Stayed")) +
    coord_flip() +
    labs(x = "", y = "Share of companies") +
    theme_bw() +
    theme(
      legend.position = "bottom", 
      legend.direction = "horizontal",
      panel.grid.minor.x = element_blank())
    
# Distance of migrations
mig_distance_plot <- migrations %>% 
  drop_na(distance) %>% 
  ggplot(aes(x = distance)) +
  geom_freqpoly(bins = 50) +
  scale_x_continuous(trans = "log10", breaks = 10^(-1:3), labels = ~ .x) +
  theme_bw(base_size = 14, base_family = "Times New Roman") +
  labs(x = "Distance of migration, km", y = "Count of companies")

# Date of migrations
# Overall timeline
mig_timeline_plot <- migrations %>% 
  mutate(yearmon = as.Date(format(date, "%Y-%m-01"))) %>% 
  ggplot(aes(x = yearmon)) +
    geom_bar() +
    scale_x_date(
      date_breaks = "6 months", date_labels = "%m.%Y", 
      guide = guide_axis(angle = 90)) +
  theme_bw() +
  labs(x = "Месяц и год", y = "Количество компаний")

# By month
mig_by_month_plot <- migrations %>% 
  mutate(month = format(date, "%m")) %>% 
  ggplot(aes(x = month)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Месяц", y = "Количество компаний")

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
  arrange(-count)

migration_msk_spb <- migration_by_region %>% 
  filter(region %in% c("Москва", "Санкт-Петербург")) %>% 
  rename(settlement = region)

# Migration by settlements
migration_by_settlement <- migrations %>% 
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
  rbind(migration_msk_spb)

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
    path = glue_collapse(sort(c(region_from, region_to)), sep = " -> "),
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
  mutate(path = ifelse(
    count >= 0,
    path,
    glue_collapse(rev(strsplit(path, " -> ", fixed = TRUE)[[1]]), sep = " -> ")),
    count = ifelse(count >= 0, count, -count))

# Centralization/Decentralization
reg_central <- regional_paths %>% 
  mutate(type = case_when(
    region_from == "Санкт-Петербург" & region_to == "Ленинградская область" ~ "from-center",
    region_from == "Ленинградская область" & region_to == "Санкт-Петербург" ~ "to-center",
    region_from == "Московская область" & region_to != "Москва" ~ "from-center",
    region_to == "Московская область" & region_from != "Москва" ~ "to-center",
    region_from == "Москва" ~ "from-center",
    region_to == "Москва" ~ "to-center",
    TRUE ~ "other"
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
    path = glue_collapse(sort(c(settlement_from, settlement_to)), sep = " -> "),
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
    glue_collapse(rev(strsplit(path, " -> ", fixed = TRUE)[[1]]), sep = " -> ")),
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
    from_capital & !to_capital ~ "from-center",
    to_capital & !from_capital ~ "to-center",
    TRUE ~ "other"
  )) %>% 
  group_by(type) %>% 
  summarise(across(count:empl, sum))
