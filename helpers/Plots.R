library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(scales)
library(sf)
library(tidyr)

# Localized labels
labels <- data.frame(
  ru = c(
    "Субъект России",
    "Год",
    "Количество субъектов МСП",
    "Сельское хозяйство",
    "Лесное хозяйство",
    "Пшеница",
    "Картофель",
    "Количество фирм",
    "Группа ОКВЭД",
    "Сельское хозяйство",
    "Лесоводство и лесозаготовки",
    "Рыболовство и рыбоводство",
    "Изменение за 2016–2023",
    "Количество фирм",
    "Число работников",
    "Прибыль"
  ),
  en = c(
    "Region",
    "Year",
    "SMBs count",
    "Agriculture",
    "Forestry",
    "Wheat",
    "Potato",
    "SMBs count",
    "Activity group",
    "Agriculture",
    "Forestry",
    "Fishery",
    "Change, 2016–2023",
    "SMBs count",
    "Employees count",
    "Total profit"
    
  )
)
labels <- labels[[LOCALE]]

# Data
rsmp_data_path <- "../tax-service-opendata/rsmp/reestr_group_A/data_product.csv"
rsmp_data <- read_csv(rsmp_data_path)

# Maps
regions_boundaries <- st_read("assets/ru.geojson")
ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")
regions <- read_csv("../tax-service-opendata/assets/regions.csv")
regions <- regions_boundaries %>% 
  left_join(regions, by = c("shapeISO" = "iso_code")) %>% 
  select(name, name_en = shapeName)
ru_svr <- st_read("assets/ru_svr.geojson")

# Regions time series
monthly_counts <- do.call(rbind, lapply(
  seq.Date(min(rsmp_data$start_date), max(rsmp_data$end_date), by = "month"),
  function(x) {
    filter(rsmp_data, start_date <= x, end_date >= x) %>% 
      count(region) %>% 
      mutate(date = x) %>% 
      arrange(-n) %>% 
      head(15)
  }
))

time_series_plot <- expand(monthly_counts, region, date) %>% 
  left_join(monthly_counts, by = c("region", "date")) %>% 
  mutate(region = fct_reorder2(region, date, n, first2)) %>% 
  filter(n > 1000) %>% 
  ggplot(aes(x = date, y = n, color = region)) +
  geom_line(na.rm = TRUE) +
  scale_y_continuous(trans = "log10") +
  labs(x = labels[2], y = labels[3]) +
  theme_bw(base_size = 12, base_family = "Times New Roman")

if (LOCALE == "en") {
  time_series_plot <- time_series_plot + 
    scale_color_discrete(
      name = labels[1],
      labels = function(breaks) {
        br <- data.frame(name = breaks)
        left_join(br, regions) %>% select(name_en) %>% pull
    })
} else {
  time_series_plot <- time_series_plot + scale_color_discrete(name = labels[1])
}
time_series_plot

# Regional distribution
ac_code_mapping <- c("01" = labels[4], "02" = labels[5])
regions_plot_agri_forestry <- rsmp_data %>%
  mutate(
    ac_start = substr(activity_code_main, 1, 2),
    ac_type = ac_code_mapping[ac_start]) %>% 
  filter(
    start_date <= "2021-12-31",
    end_date >= "2021-12-31",
    !is.na(ac_type)) %>% 
  count(region, ac_type) %>% 
  group_by(ac_type) %>% 
  mutate(n = rescale(n)) %>% 
  left_join(regions, by = c("region" = "name")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = n), size = .1) +
  coord_sf(crs = ru_crs) +
  scale_fill_distiller(
    name = NULL,
    labels = function(breaks) c("Min", rep("", length(breaks) - 2), "Max"),
    palette = "YlGn",
    direction = 1) +
  facet_wrap(~ac_type) +
  theme_bw(base_size = 12, base_family = "Times New Roman") +
  theme(legend.position = "bottom")
regions_plot_agri_forestry

ac_code_mapping_wp <- c("01.11.11" = labels[6], "01.13.31" = labels[7])
regions_plot_wheat_potato <- rsmp_data %>%
  mutate(
    ac_type = ac_code_mapping_wp[activity_code_main]) %>% 
  filter(
    start_date <= "2021-12-31",
    end_date >= "2021-12-31",
    !is.na(ac_type)) %>% 
  count(region, ac_type) %>% 
  group_by(ac_type) %>% 
  mutate(n = rescale(n)) %>% 
  pivot_wider(names_from = ac_type, values_from = n) %>% 
  right_join(regions, by = c("region" = "name")) %>% 
  pivot_longer(cols = c(labels[6], labels[7]), names_to = "ac_type", values_to = "n") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = n), size = .1) +
  coord_sf(crs = ru_crs) +
  scale_fill_distiller(
    name = NULL,
    labels = function(breaks) c("Min", rep("", length(breaks) - 2), "Max"),
    palette = "YlGn",
    direction = 1) +
  facet_wrap(~ac_type) +
  theme_bw(base_size = 12, base_family = "Times New Roman") + 
  theme(legend.position = "bottom")
regions_plot_wheat_potato

# Distribution by settlements
activity_by_settlements_df <- rsmp_data %>%
  mutate(ac_type = substr(activity_code_main, 1, 2)) %>% 
  filter(
    start_date <= "2021-12-31",
    end_date >= "2021-12-31"
  ) %>% 
  count(region, area, settlement, lat, lon, ac_type) %>% 
  drop_na(lat, lon) %>% 
  filter(n > 50) %>% 
  group_by(region, area, settlement, lat, lon,) %>% 
  slice_max(n) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

activity_by_settlements <- regions %>% ggplot() +
  geom_sf(size = .1) +
  geom_sf(data = activity_by_settlements_df, aes(color = ac_type, size = n)) +
  coord_sf(crs = ru_crs) +
  scale_size_continuous(name = labels[8]) +
  scale_color_discrete(name = labels[9], labels = c(labels[10], labels[11], labels[12])) +
  theme_bw(base_size = 12, base_family = "Times New Roman") +
  theme(legend.position = "bottom", legend.direction = "vertical")
activity_by_settlements

ru_svr_activity <- rsmp_data %>%
  mutate(ac_type = substr(activity_code_main, 1, 2)) %>% 
  filter(
    region == "Свердловская область",
    start_date <= "2021-12-31",
    end_date >= "2021-12-31"
  ) %>% 
  count(region, area, settlement, lat, lon, ac_type) %>% 
  drop_na() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 

activity_by_settlements_svr <- ru_svr %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = ru_svr_activity, aes(color = ac_type, size = n)) +
  coord_sf(crs = 4326) +
  scale_size_continuous(name = labels[8]) +
  scale_color_discrete(name = labels[9], labels = c(labels[10], labels[11], labels[12])) +
  theme_bw(base_size = 12, base_family = "Times New Roman")
activity_by_settlements_svr

# Migrations
region_changes <- rsmp_data %>% 
  select(tin, region) %>% 
  group_by(tin) %>% 
  filter(n() > 1) %>% 
  mutate(next_region = lead(region)) %>% 
  drop_na(next_region) %>% 
  filter(region != next_region) %>% 
  select(from = region, to = next_region) %>% 
  pivot_longer(from:to, names_to = "option", values_to = "region")

migrations_plot <- region_changes %>% ungroup() %>% 
  count(region, option) %>% 
  pivot_wider(names_from = option, values_from = n) %>% 
  mutate(change = to - from) %>% 
  right_join(regions, by = c("region" = "name")) %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = change), size = .1) + 
  coord_sf(crs = ru_crs) +
  scale_fill_gradient2(name = labels[13], low = "#d01c8b", high = "#4dac26") +
  theme_bw(base_size = 12, base_family = "Times New Roman") +
  theme(legend.position = "bottom")
migrations_plot

# With revexp and employees
revexp_data_path <- "../tax-service-opendata/revexp/csv/group_A.csv"
revexp_data <- read_csv(revexp_data_path)
sshr_data_path <- "../tax-service-opendata/sshr/csv/group_A.csv"
sshr_data <- read_csv(sshr_data_path)

count_profit_employees_by_region_plot <- rsmp_data %>%
  filter(
    start_date <= "2021-12-31",
    end_date >= "2021-12-31",
    substr(activity_code_main, 1, 2) == "01"
  ) %>% 
  left_join(filter(revexp_data, year == 2021), by = "tin") %>% 
  left_join(filter(sshr_data, year == 2021), by = "tin") %>% 
  mutate(profit = revenue - expediture) %>% 
  select(tin, region, profit, employees_count) %>% 
  group_by(region) %>% 
  summarise(
    count = n(),
    profit = sum(profit, na.rm = TRUE),
    employees = sum(employees_count, na.rm = TRUE)
  ) %>% 
  pivot_longer(-region, names_to = "option", values_to = "value") %>% 
  right_join(regions, by = c("region" = "name")) %>% 
  st_as_sf() %>% 
  group_by(option) %>% 
  mutate(value = rescale(value)) %>% 
  ungroup() %>% 
  mutate(option = factor(option, levels = c("count", "employees", "profit"), labels = c(labels[14], labels[15], labels[16]))) %>% 
  ggplot() +
  geom_sf(aes(fill = value), size = .1) +
  coord_sf(crs = ru_crs) +
  facet_wrap(~option, ncol = 2) +
  scale_fill_distiller(name = "", palette = "YlGn", direction = 1, labels = c("min", "", "", "", "max")) +
  theme_bw(base_size = 12, base_family = "Times New Roman") +
  theme(legend.position = "bottom")
count_profit_employees_by_region_plot
