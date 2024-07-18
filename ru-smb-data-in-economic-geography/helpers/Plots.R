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
    "Рис",
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
labels <- labels[["ru"]]

# Data
rsmp_data_path <- "../ru-smb-companies/group_A/smb.csv"
rsmp_data <- read_csv(rsmp_data_path)

# Maps
regions_boundaries <- st_read("assets/ru.geojson")
ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")
regions <- read_csv("assets/regions.csv")
regions <- regions_boundaries %>% 
  left_join(regions, by = c("shapeISO" = "iso_code")) %>% 
  select(name, name_en = shapeName)
ru_svr <- st_read("assets/ru_svr.geojson")

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

ac_code_mapping_wr <- c("01.11.1" = labels[6], "01.12" = labels[7])
regions_plot_wheat_rice <- rsmp_data %>%
  mutate(
    ac_type = ac_code_mapping_wr[activity_code_main]) %>% 
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
regions_plot_wheat_rice

# Distribution by settlements
activity_by_settlements_df <- rsmp_data %>%
  mutate(ac_type = substr(activity_code_main, 1, 2)) %>% 
  filter(
    start_date <= "2021-12-31",
    end_date >= "2021-12-31"
  ) %>% 
  count(region, area, settlement, lat, lon, ac_type) %>% 
  drop_na(lat, lon) %>% 
  mutate(
    lon = if_else(lon < 0, -(180 - lon), lon)
  ) %>% 
  group_by(region, area, settlement, lat, lon) %>% 
  slice_max(n) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

activity_by_settlements <- regions %>%
  ggplot() +
  geom_sf(size = .1) +
  geom_sf(data = activity_by_settlements_df, 
          aes(color = ac_type), size = 1, shape = 1) +
  coord_sf(crs = ru_crs) +
  scale_color_discrete(
    name = labels[9], 
    labels = c(labels[10], labels[11], labels[12])) +
  theme_bw(base_size = 11, base_family = "Times New Roman") +
  theme(
    legend.position = "bottom", legend.direction = "horizontal",
    legend.title.position = "top")
activity_by_settlements

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