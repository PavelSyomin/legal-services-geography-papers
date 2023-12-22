library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(scales)
library(sf)
library(tidyr)

rsmp_data_path <- "../tax-service-opendata/rsmp/reestr_group_A/data_product.csv"
rsmp_data <- read_csv(rsmp_data_path)

rsmp_data %>% 
  count(region, start_date, end_date)
# Regions time series
monthly_counts <- do.call(rbind, lapply(
  seq.Date(min(rsmp_data$start_date), max(rsmp_data$end_date), by = "month"),
  function(x) {
    filter(rsmp_data, start_date <= x, end_date >= x) %>% 
      count(region) %>% 
      mutate(date = x) %>% 
      arrange(-n)
  }
))

time_series_plot <- expand(monthly_counts, region, date) %>% 
  left_join(monthly_counts, by = c("region", "date")) %>% 
  mutate(region = fct_reorder2(region, date, n, first2)) %>% 
  filter(n > 1000) %>% 
  ggplot(aes(x = date, y = n, color = region)) +
  geom_line(na.rm = TRUE, show.legend = FALSE) +
  #scale_x_continuous(limits = c(2016, 2024)) + 
  scale_y_continuous(trans = "log10") +
  scale_color_grey(name = "Субъект России") +
  labs(x = "Год", y = "Количество субъектов МСП") +
  theme_bw()
time_series_plot

# Maps
regions_boundaries <- st_read("geoBoundaries-RUS-ADM1_simplified.geojson")
ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")
regions <- read_csv("../tax-service-opendata/assets/regions.csv")

regions <- regions_boundaries %>% 
  left_join(regions, by = c("shapeISO" = "iso_code")) %>% 
  select(name)

ac_code_mapping <- c("01" = "Сельское хозяйство", "02" = "Лесное хозяйство")
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
  facet_wrap(~ac_type)
regions_plot_agri_forestry

ac_code_mapping_wp <- c("01.11.11" = "Пшеница", "01.13.31" = "Картофель")
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
  pivot_longer(cols = "Картофель":"Пшеница", names_to = "ac_type", values_to = "n") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = n), size = .1) +
  coord_sf(crs = ru_crs) +
  scale_fill_distiller(
    name = NULL,
    labels = function(breaks) c("Min", rep("", length(breaks) - 2), "Max"),
    palette = "YlGn",
    direction = 1) +
  facet_wrap(~ac_type)
regions_plot_wheat_potato

activity_by_settlements <- rsmp_data %>%
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

activity_by_settlements <- ggplot(regions) +
  geom_sf(size = .1) +
  geom_sf(data = activity_by_settlements, aes(color = ac_type, size = n)) +
  coord_sf(crs = ru_crs) +
  scale_size_continuous(name = "Количество фирм") +
  scale_color_discrete(name = "Группа ОКВЭД", labels = c("Сельское хозяйство", "Лесоводство и лесозаготовки", "Рыболовство и рыбоводство"))
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

ru_svr <- st_read("ru_svr.geojson")
activity_by_settlements_svr <- ru_svr %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = ru_svr_activity, aes(color = ac_type, size = n)) +
  coord_sf(crs = 4326) +
  scale_size_continuous(name = "Количество фирм") +
  scale_color_discrete(name = "Группа ОКВЭД", labels = c("Сельское хозяйство", "Лесоводство и лесозаготовки", "Рыболовство и рыбоводство"))
activity_by_settlements_svr


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
  scale_fill_distiller(palette = "PiYG", direction = 1)
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
  ggplot() +
  geom_sf(aes(fill = value), size = .1) +
  coord_sf(crs = ru_crs) +
  facet_wrap(~option, ncol = 2) +
  scale_fill_distiller(palette = "YlGn", direction = 1)
count_profit_employees_by_region_plot