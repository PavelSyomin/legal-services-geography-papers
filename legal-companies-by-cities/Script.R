library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(tidyr)

# Data on firms, and cities lookup table
panel <- read_csv("../../ru-smb-companies/legal/panel.csv")
cities <- rbind(
  read_csv("cities.csv"),
  read_csv("cities_additional.csv")
)
regions_geo <- st_read("ru.geojson")

lcc <- panel %>% 
  filter(
    kind == 1, 
    year == 2021, 
    settlement_type == "г",
    revenue > 0,
    expenditure > 0
  ) %>% 
  mutate(city = case_when(
    region == "Москва" & is.na(settlement) ~ "Москва",
    region == "Санкт-Петербург" & is.na(settlement) ~ "Санкт-Петербург",
    TRUE ~ settlement
  )) %>% 
  select(region, city, oktmo, empl = employees_count) %>% 
  replace_na(list(empl = 0)) %>% 
  mutate(empl = replace(empl, empl == 0, 1)) %>% 
  group_by(region, city, oktmo) %>% 
  summarise(count = n(), empl = sum(empl), .groups = "drop")

cp <- cities %>% 
  mutate(city = coalesce(cities$city, cities$area, cities$region)) %>% 
  select(city, oktmo, population, lat = geo_lat, lon = geo_lon)

data <- left_join(lcc, cp, by = c("oktmo", "city")) %>% 
  drop_na(count, empl, population) %>% 
  mutate(
    count_per_100k = 1e5 * count / population,
    empl_per_100k = 1e5 * empl / population,
    concentration = empl / sum(empl),
    city_size_group = cut(
      population,
      breaks = c(0, 5e4, 1e5, 2.5e5, 5e5, 1e6, 1e9),
      labels = c(
        "Small (<50k)", "Medium (50–100k)", "Big (100–250k)",
        "Large (250–500k)", "Extra-large (500k–1M)", 
        "Millionare (>1M)")
    )) %>% 
  filter(
    !(city %in% c("Иннополис", "Кировск", "Бронницы", "Красноармейск"))) %>% 
  group_by(region) %>% 
  mutate(
    city_region_rank = row_number(-population),
    is_largest_in_region = city_region_rank == 1) %>% 
  ungroup()

ggplot(data, aes(
    x = empl_per_100k, y = concentration, 
    color = city_size_group, shape = is_largest_in_region)) +
  geom_point(size = 2) +
  scale_x_log10(name = "Lawyers per 100k population (log10)") +
  scale_y_log10(name = "Concentration (log10)") +
  scale_color_brewer(name = "City size by population", palette = "Dark2") +
  scale_shape_manual(
    name = "", 
    values = c("TRUE" = 19, "FALSE" = 21),
    labels = c(
      "TRUE" = "The largest city in a region", 
      "FALSE" = "Other cities")) +
  theme_bw(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = c(.01, .99),
    legend.justification = c(0, 1),
    panel.grid.minor.y = element_blank())

data %>% 
  pivot_longer(
    cols = c("count", "empl"),
    names_to = "var",
    values_to = "val") %>% 
  ggplot(aes(x = population, y = val)) +
  geom_point(shape = 21, color = "gray10") +
  geom_smooth(method = "lm", linetype = "dashed", color = "red", se = FALSE) + 
  scale_x_log10(name = "City population", breaks = 10 ** c(4:7), labels = c("10K", "100K", "1M", "10M")) +
  scale_y_log10(name = "Count") +
  facet_wrap(
    ~var, ncol = 2, 
    labeller = as_labeller(
      c("count" = "Companies", "empl" = "Employees"))) +
  theme_bw(base_size = 14, base_family = "Times New Roman")

data %>% 
  pivot_longer(
    cols = c("count", "empl"),
    names_to = "var",
    values_to = "val") %>%
  group_by(var) %>% 
  mutate(
    rank = row_number(-val)) %>% 
  ungroup() %>% 
  ggplot(aes(x = val, y = rank)) +
  geom_line() +
  geom_point(size = .1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  scale_x_log10(name = "Count") +
  scale_y_log10(name = "Rank") +
  facet_wrap(~var, ncol = 2, labeller = as_labeller(
    c("count" = "Companies", "empl" = "Employees"))) +
  theme_bw(base_size = 14, base_family = "Times New Roman")

rank_empl_lm <- data %>% 
  mutate(rank = row_number(-empl)) %>% 
  lm(log(rank) ~ log(empl), .)
summary(rank_empl_lm)

rank_count_lm <- data %>% 
  mutate(rank = row_number(-count)) %>% 
  lm(log(rank) ~ log(count), .)
summary(rank_count_lm)

ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")
data %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
  ggplot() +
  geom_sf(data = regions_geo, color = "gray50", fill = "white") +
  geom_sf(aes(color = empl_per_100k, size = city_size_group), shape = 19) +
  coord_sf(crs = ru_crs) +
  scale_color_binned(name = "Lawyers per 100k", n.breaks = 4, low = "#dadaeb", high = "#3f007d") +
  scale_size_discrete(name = "City size", range = c(.2, 2)) +
  theme_bw(base_size = 14, base_family = "Times New Roman")
