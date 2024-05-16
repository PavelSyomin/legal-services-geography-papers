library(dplyr)
library(ggplot2)
library(glue)
library(here)
library(readr)
library(sf)
library(tidyr)

# Load data
panel <- read_csv(here("../../ru-smb-companies/legal/panel.csv"))
cities <- rbind(
  read_csv(here("common", "cities.csv")),
  read_csv(here("common", "cities_additional.csv"))
)
regions_geo <- st_read(here("common", "ru.geojson"))
tiles <- read_csv(here("journal-article", "russia-tiles.csv"))

# Build table with number of employees by city
# lfc means Law Firms by City
lfc <- panel %>% 
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

# Build table with cities population
# cp means Cities Population
cp <- cities %>% 
  mutate(
    city = coalesce(cities$city, cities$area, cities$region),
    is_regional_center = case_when(
      city == "Москва" ~ TRUE,
      city == "Санкт-Петербург" ~ TRUE,
      capital_marker == 2 ~ TRUE,
      TRUE ~ FALSE
    )) %>% 
  select(city, oktmo, population, 
         lat = geo_lat, lon = geo_lon,
         is_regional_center)

# Make joint data table for analysis
data <- left_join(lfc, cp, by = c("oktmo", "city")) %>% 
  drop_na(count, empl, population) %>% 
  mutate(
    provision = 1e4 * empl / population,
    size = cut(
      population,
      breaks = c(0, 5e4, 1e5, 2.5e5, 5e5, 1e6, 1e8),
      labels = c(
        "Small (<50k)", "Medium (50–100k)", "Big (100–250k)",
        "Large (250–500k)", "Extra-large (500k–1M)", 
        "Millionaire (>1M)")
    )) %>% 
  group_by(region) %>% 
  ungroup()

# Look at the regression variables extreme values
# to identify outliers
arrange(data, -empl) %>% select(city, empl)
arrange(data, -provision) %>% select(city, provision)
data %>% 
  ggplot(aes(x = empl, y = provision, label = city)) +
  geom_point() +
  geom_text(check_overlap = TRUE)
outliers <- c("Иннополис", "Кировск", "Бронницы", "Красноармейск", 
              "Москва", "Санкт-Петербург")

ep_with_outliers_plot <- data %>% 
  mutate(
    is_msk_spb = city %in% c("Москва", "Санкт-Петербург")) %>% 
  filter(!(city %in% outliers) | is_msk_spb) %>%
  ggplot(aes(x = empl, y = provision)) +
  geom_point(
    aes(color = is_msk_spb), 
    size = 1,
    shape = 21) +
  geom_text(
    data = ~ filter(.x, is_msk_spb) %>% mutate(city = if_else(city == "Москва", "Msk", "SPb")), 
    aes(label = city),
    vjust = "inward",
    hjust = "inward") +
  scale_color_manual(
    name = NULL,
    values = c("TRUE" = "red1", "FALSE" = "gray50"),
    guide = guide_none()) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    panel.grid.minor = element_blank())
ep_with_outliers_plot

# Make the main plot: provision ~ empl grouped by size
ep_plot <- data %>% 
  filter(!(city %in% outliers)) %>% 
  ggplot(aes(x = empl, y = provision)) +
  geom_point(
    aes(color = size, shape = is_regional_center), 
    size = 2,
    alpha = .8
  ) +
  geom_smooth(
    aes(color = size, linetype = "solid"), 
    method = "lm", 
    se = FALSE,
    linewidth = .5,
    alpha = 1
  ) +
  geom_smooth(
    aes(linetype = "dotted"),
    method = "lm", 
    formula = y ~ sqrt(x),
    se = FALSE,
    color = "gray80",
    linewidth = 1,
    alpha = .75
  ) +
  annotation_custom(
    ggplotGrob(ep_with_outliers_plot), 
    xmin = 750, 
    ymax = 5) +
  scale_shape_manual(
    name = "City type", 
    values = c("TRUE" = 8, "FALSE" = 21),
    labels = c(
      "TRUE" = "Regional center", 
      "FALSE" = "Regular city"),
    guide = guide_legend(order = 1)) +
  scale_linetype_manual(
    name = "Linear fits",
    values = c("dotted", "solid"),
    labels = c("solid" = "By city group", "dotted" = "Overall"),
    guide = guide_legend(
      override.aes = list(color = "gray50"),
      order = 3
    )
  ) +
  scale_color_brewer(
    name = "City size by population", palette = "Dark2",
    guide = guide_legend(order = 2)) +
  theme_bw(base_size = 14) +
  theme(
    legend.spacing.y = unit(0, "cm"),
    panel.grid.minor.y = element_blank()) +
  labs(x = "Lawyers in a city (n)", y = "Lawyers per 10,000 (p)")
ep_plot

# Make a binned version of the main plot
ep_binned_plot <- data %>% 
  filter(!(city %in% outliers)) %>% 
  ggplot(aes(x = empl, y = provision)) +
  geom_bin2d(binwidth = c(250, 5)) +
  geom_point(color = "gray50", shape = 21, size = 2) +
  stat_bin2d(
    aes(label = sprintf("%.1f%%", 100 * after_stat(density))),
    geom = "text", size = 5,
    binwidth = c(250, 5)
  ) +
  scale_fill_binned(
    name = "# of cities in a bin",
    high = "#41ab5d", low = "#f7fcf5",
    transform = "log10") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(.99, .01),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.spacing.y = unit(0, "cm"),
    panel.grid.minor.y = element_blank()) +
  labs(x = "Lawyers in a city (n)", y = "Lawyers per 10,000 (p)")
ep_binned_plot

# Make a map with provision > 5
ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")
map <- data %>% 
  filter(!(city %in% outliers), provision >= 5) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
  ggplot() +
  geom_sf(data = regions_geo, color = "gray50", fill = "white") +
  geom_sf(aes(color = provision), shape = 19) +
  coord_sf(crs = ru_crs) +
  scale_color_binned(
    name = "Lawers per 10,000 people",
    n.breaks = 4,
    low = "#74c476",
    high = "#00441b") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
map

# Theoretical models
facet_labels <- c(
  "Type 1: s = 0",
  "Type 2: s > 0",
  "Type 3: s < 0",
  "Type 4: s → ∞")
lines <- data.frame(
  x = c(0, 1, 0, 1, 0, 1, 0.5, 0.5),
  y = c(1, 1, 0, 1, 1, 0, 0, 1),
  group = factor(
    c(1, 1, 2, 2, 3, 3, 4, 4),
    levels = 1:4, 
    labels = facet_labels
  )
)
data_points <- rbind(
  tibble(
    x = runif(20),
    y = jitter(rep(1, 20), 5),
    group = 1
  ),
  tibble(
    x = runif(20),
    y = x + jitter(rep(0, 20), 5),
    group = 2
  ),
  tibble(
    x = runif(20),
    y = 1 - (x + jitter(rep(0, 20), 5)),
    group = 3
  ),
  tibble(
    x = jitter(rep(0.5, 20), 5),
    y = runif(20),
    group = 4
  )
) %>% 
  mutate(group = factor(group, 1:4, labels = facet_labels))

theoretical_models_plot <- ggplot(lines, aes(x = x, y = y)) +
  geom_path() +
  geom_point(data = data_points, size = .5, alpha = .3) +
  scale_x_continuous(
    name = "Number of lawyers (n)",
    labels = function (br) br * 1000,
    limits = c(-.1, 1.1)
  ) +
  scale_y_continuous(
    name = "Laywers per 100,000 (p)",
    labels = function (br) br * 10,
    limits = c(-.1, 1.1)
  ) +
  facet_wrap(vars(group), nrow = 1) +
  coord_fixed() +
  theme_minimal(base_size = 14) +
  labs(caption = "s means slope, dots are hypothetical data points,\nline is hypothetical linear fit of points")
theoretical_models_plot


