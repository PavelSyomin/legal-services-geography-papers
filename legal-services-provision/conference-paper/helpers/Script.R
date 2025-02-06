library(dplyr)
library(ggplot2)
library(glue)
library(here)
library(readr)
library(sf)
library(tidyr)

# Data on firms, and cities lookup table
panel <- read_csv(here("../large-datasets/law-firms/panel.csv"))
cities <- rbind(
  read_csv(here("common", "cities.csv")),
  read_csv(here("common", "cities_additional.csv"))
)
regions_geo <- st_read(here("common", "ru.geojson"))

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
        "Millionaire (>1M)")
    )) %>% 
  filter(
    !(city %in% c("Иннополис", "Кировск", "Бронницы", "Красноармейск"))) %>% 
  group_by(region) %>% 
  mutate(
    city_region_rank = row_number(-population),
    is_largest_in_region = city_region_rank == 1) %>% 
  ungroup()

ca_model <- lm(log(empl_per_100k) ~ log(concentration), data)
ca_model_summary <- summary(ca_model)
ca_model_formula = glue(
  "log10(availability) = {intercept} {sign} {slope} × log10(concentration)",
  intercept = round(ca_model_summary$coefficients[1, 1], 2), 
  slope = round(ca_model_summary$coefficients[2, 1], 2),
  sign = substr(sprintf("%+ .2f", ca_model_summary$coefficients[2, 1]), 1, 1))
ca_model_formula

ca_plot <- ggplot(
    data, 
    aes(x = concentration, y = empl_per_100k)
  ) +
  geom_text(
    aes(label = c("Msk", "SPb")),
    data = filter(data, city %in% c("Москва", "Санкт-Петербург")),
    vjust = -1,
    hjust = 1,
  ) +
  geom_point(
    aes(color = city_size_group, shape = is_largest_in_region), 
    size = 2
  ) +
  geom_smooth(
    aes(color = city_size_group), 
    data = filter(data, ! (city %in% c("Москва", "Санкт-Петербург"))),
    method = "lm", 
    se = FALSE,
    linetype = "dotted",
    size = 1,
    alpha = .75
  ) +
  geom_smooth(
    data = filter(data, ! (city %in% c("Москва", "Санкт-Петербург"))),
    method = "lm", 
    se = FALSE,
    color = "gray80",
    size = 1,
    alpha = .75
  ) +
  scale_x_log10(name = "Concentration (log10)") +
  scale_y_log10(name = "Availability (log10)") +
  scale_color_brewer(
    name = "City size by population", palette = "Dark2") +
  scale_shape_manual(
    name = NULL, 
    values = c("TRUE" = 19, "FALSE" = 21),
    labels = c(
      "TRUE" = "The largest city in a region", 
      "FALSE" = "Other cities")) +
  theme_bw(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = c(.99, .01),
    legend.justification = c(1, 0),
    legend.spacing.y = unit(0, "cm"),
    panel.grid.minor.y = element_blank())
ca_plot

ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")
spatial_plot <- data %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
  ggplot() +
  geom_sf(data = regions_geo, color = "gray50", fill = "white") +
  geom_sf(aes(color = empl_per_100k, size = city_size_group), shape = 19) +
  coord_sf(crs = ru_crs) +
  scale_color_binned(name = "Availability", n.breaks = 4, low = "#dadaeb", high = "#3f007d") +
  scale_size_discrete(name = "City size by population", range = c(.2, 2)) +
  theme_bw(base_size = 14, base_family = "Times New Roman")
spatial_plot
