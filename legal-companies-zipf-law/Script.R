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
  filter(city != "Иннополис") %>% 
  group_by(region) %>% 
  mutate(
    city_region_rank = row_number(-population),
    is_largest_in_region = city_region_rank == 1) %>% 
  ungroup()

counts_and_population_plot <- data %>% 
  pivot_longer(
    cols = c("count", "empl"),
    names_to = "var",
    values_to = "val") %>% 
  ggplot(aes(x = population, y = val)) +
  geom_point(shape = 21, color = "gray10") +
  geom_smooth(method = "lm", linetype = "dashed", color = "red", se = FALSE) + 
  scale_x_log10(name = "Население города", breaks = 10 ** c(4:7), labels = c("10K", "100K", "1M", "10M")) +
  scale_y_log10(name = "Количество") +
  facet_wrap(
    ~var, ncol = 2, 
    labeller = as_labeller(
      c("count" = "(а) Компании", "empl" = "(б) Работники"))) +
  theme_bw(base_size = 14, base_family = "Times New Roman")
counts_and_population_plot

count_pop_lm <- lm(log(count) ~ log(population), data)
summary(count_pop_lm)
count_pop_rsq <- summary(count_pop_lm)$adj.r.squared

empl_pop_lm <- lm(log(empl) ~ log(population), data)
summary(empl_pop_lm)
empl_pop_rsq <- summary(empl_pop_lm)$adj.r.squared

rank_size_plot <- data %>% 
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
  scale_x_log10(name = "Количество") +
  scale_y_log10(name = "Ранг") +
  facet_wrap(~var, ncol = 2, labeller = as_labeller(
    c("count" = "(а) Компании", "empl" = "(б) Работники"))) +
  theme_bw(base_size = 14, base_family = "Times New Roman")
rank_size_plot

rank_count_lm <- data %>% 
  mutate(rank = row_number(-count)) %>% 
  lm(log(rank) ~ log(count), .)
summary(rank_count_lm)
rank_count_rsq <- summary(rank_count_lm)$adj.r.squared

rank_empl_lm <- data %>% 
  mutate(rank = row_number(-empl)) %>% 
  lm(log(rank) ~ log(empl), .)
summary(rank_empl_lm)
rank_empl_rsq <- summary(rank_empl_lm)$adj.r.squared


