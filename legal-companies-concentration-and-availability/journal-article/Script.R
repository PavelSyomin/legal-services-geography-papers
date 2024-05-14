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
    city_group = cut(
      population,
      breaks = c(0, 5e4, 1e5, 2.5e5, 5e5, 1e6, 1e9),
      labels = c(
        "Small (<50k)", "Medium (50–100k)", "Big (100–250k)",
        "Large (250–500k)", "Extra-large (500k–1M)", 
        "Millionaire (>1M)")
    )) %>% 
  group_by(region) %>% 
  mutate(
    city_region_rank = row_number(-population),
    is_largest_in_region = city_region_rank == 1) %>% 
  ungroup() %>% 
  select(-city_region_rank)

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


ca_model <- lm(log(empl_per_100k) ~ log(empl), data)
ca_model_summary <- summary(ca_model)
ca_model_formula = glue(
  "log10(availability) = {intercept} {sign} {slope} × log10(concentration)",
  intercept = round(ca_model_summary$coefficients[1, 1], 2), 
  slope = round(ca_model_summary$coefficients[2, 1], 2),
  sign = substr(sprintf("%+ .2f", ca_model_summary$coefficients[2, 1]), 1, 1))
ca_model_formula

ca_plot <- data %>% 
  filter(!(city %in% outliers)) %>% 
  ggplot(aes(x = empl, y = provision)) +
  geom_point(
    aes(color = city_group, shape = is_regional_center), 
    size = 2,
    alpha = .8
  ) +
  geom_smooth(
    aes(color = city_group, linetype = "solid"), 
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
ca_plot

data %>% 
  filter(!(city %in% outliers)) %>% 
  ggplot(aes(x = empl, y = provision)) +
  geom_bin2d(binwidth = c(250, 5)) +
  geom_point(color = "gray80", size = .5) +
  stat_bin2d(
    aes(label = sprintf("(%.1f%%)", 100 * after_stat(density))),
    geom = "text", size = 3, color = "gray50",
    binwidth = c(250, 5),
    vjust = 1,
  ) +
  stat_bin2d(
    aes(label = after_stat(count)),
    geom = "text", size = 5,
    binwidth = c(250, 5),
  ) +
  scale_fill_binned(
    name = "",
    high = "#41ab5d", low = "#f7fcf5",
    transform = "log10") +
  theme_bw(base_size = 14) +
  theme(
    legend.spacing.y = unit(0, "cm"),
    panel.grid.minor.y = element_blank()) +
  labs(x = "Lawyers in a city (n)", y = "Lawyers per 10,000 (p)")

data %>% 
  filter(!(city %in% outliers)) %>% 
  ggplot(aes(x = empl, y = provision)) +
  geom_point(
    aes(color = city_group, alpha = if_else(provision > 5, 1, .3)), 
    size = 1,
  ) +
  scale_color_brewer(
    name = "City size by population", palette = "Dark2",
    guide = guide_legend(order = 2)) +
  theme_bw(base_size = 14) +
  theme(
    legend.spacing.y = unit(0, "cm"),
    panel.grid.minor.y = element_blank()) +
  labs(x = "Lawyers in a city (n)", y = "Lawyers per 10,000 (p)")

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
    name = "Number of law firms (LF) employees (n)",
    labels = function (br) br * 1000,
    limits = c(-.1, 1.1)
  ) +
  scale_y_continuous(
    name = "LF per 100,000 (p)",
    labels = function (br) br * 10,
    limits = c(-.1, 1.1)
  ) +
  facet_wrap(vars(group), nrow = 1) +
  coord_fixed() +
  theme_minimal(base_size = 16) +
  labs(caption = "s means slope, dots are hypothetical data points, line is hypothetical linear fit of points")
theoretical_models_plot

# y = k * x + b -> log(y) ~ log(x) = ?
x <- seq(1, 10001, length.out = 10000)
k <- 5
b <- 0
y <- k * x + b

ggplot(
  data = rbind(
    tibble(x = x, y = y, option = "y ~ x"),
    tibble(x = log10(x), y = y, option = "y ~ log(x)"),
    tibble(x = log10(x), y = log10(y), option = "log(y) ~ log(x)")
  ),
  mapping = aes(x = x, y = y)
) +
  geom_point(size = .1) +
  facet_wrap(vars(option), ncol = 1, scales = "free")

# log(y) = k * log(x) + b -> y ~ x = ?
log_x <- seq(0, 10, length.out = 10000)
k <- 1
b <- 0.6
log_y <- k * log_x + b

ggplot(
  data = rbind(
    tibble(x = log_x, y = log_y, option = "log(y) ~ log(x)"),
    tibble(x = 10 ** log_x, y = 10 ** log_y, option = "y ~ x")
  ),
  mapping = aes(x = x, y = y)
) +
  geom_point(size = .1) +
  facet_wrap(vars(option), ncol = 1, scales = "free")

data$city_size_group

fit1 <- lm(empl_per_100k ~ empl, filter(data, city_size_group == "Small (<50k)"))
summary(fit1)
fit2 <- lm(empl_per_100k ~ empl, filter(data, city_size_group == "Big (100–250k)"))
summary(fit2)
fit1_log <- lm(log10(empl_per_100k) ~ log10(empl), filter(data, city_size_group == "Small (<50k)"))
summary(fit1_log)
fit2_log <- lm(log10(empl_per_100k) ~ log10(empl), filter(data, city_size_group == "Big (100–250k)"))
summary(fit2_log)
plot(fit1_log)

fit3 <- lme(provision ~ empl, data = data, random = ~ empl | city_group)
summary(fit3)


data %>% 
  filter(!(city %in% outliers)) %>%
  right_join(tiles, by = c("region" = "name")) %>% 
  ggplot(aes(x = empl, y = provision)) +
  geom_point(aes(shape = is_regional_center), size = .2) +
  geom_text(
    data = tiles,
    aes(x = 0, y = 20, label = code_en),
    size = 2.5,
    hjust = 0,
    vjust = 1) +
  facet_grid(rows = vars(row), cols = vars(col), scales = "free") +
  #guides(color = guide_legend(ncol = 2, order = 1), alpha = guide_legend(order = 2)) +
  #theme_void(base_size = 11) +
  theme(
    aspect.ratio = 1,
    legend.position = c(1, .05),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.text = element_text(size = 11),
    panel.spacing = unit(1, "mm"),
    strip.background = element_blank(),
    strip.text = element_blank()
  )
