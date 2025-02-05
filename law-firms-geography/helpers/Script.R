library(dplyr)
library(ggplot2)
library(here)
library(readr)
library(tidyr)

# Load raw data
courts <- read_csv(here("assets/courts.csv"))
data <- read_csv(here("../large-datasets/law-firms/panel.csv"))
empl <- read_csv(here("assets/empl.csv"))
pop <- read_csv(here("assets/population.csv"))
tiles <- read_csv(here("assets/russia-tiles.csv"))

# Common preparations
data <- data %>% 
  filter(year < 2022) %>% 
  mutate(kind = replace(kind, kind == 3, 2)) %>% 
  select(tin, kind, category, region, year, empl = employees_count)

courts_by_region <- count(courts, region, name = "n_courts") %>% 
  filter(region != "Севастополь") # we don't have firms data anyway
empl <- pivot_longer(
  empl, "2016":"2021", names_to = "year", values_to = "empl_total") %>% 
  mutate(year = as.numeric(year))
pop <- pivot_longer(
  pop, "2016":"2021", names_to = "year", values_to = "pop_total") %>% 
  mutate(year = as.numeric(year))

# Relative count of law firms by region
counts_map_data <- count(data, region, year) %>% 
  left_join(pop) %>% 
  mutate(share = 1e5 * n / pop_total) %>% 
  group_by(region) %>% 
  summarise(share = median(share)) %>% 
  right_join(tiles, by = c("region" = "name"))
counts_map_breaks <- quantile(counts_map_data$share, seq(0, 1, .2), na.rm = TRUE)
counts_map <- counts_map_data %>% 
  ggplot(aes(x = col, y = -row, fill = share)) +
  geom_raster() +
  geom_text(
    aes(x = col - .4, y = -row + .3, label = code_en), 
    family = "Times New Roman", 
    size = 3,
    hjust = 0) +
  geom_text(
    aes(x = col - .4, y = -row - .3, label = round(share, 1)), 
    family = "Times New Roman", 
    size = 3,
    color = "gray20",
    hjust = 0,
    na.rm = TRUE) +
  scale_fill_binned(
    name = "Median number of law firms\nper 100,000 people (2016–21)\nScale breaks are 0.2-spaced quantiles\nNumbers on the map are exact values\nGrey fill is no data",
    breaks = counts_map_breaks,
    labels = round(counts_map_breaks, 0),
    high = "#eff3ff", low = "#2171b5"
  ) +
  coord_fixed(expand = FALSE) +
  theme_void(base_family = "Times New Roman", base_size = 11) +
  theme(
    legend.position = c(.9, .01),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.title.position = "top"
  )
counts_map

# Shares of companies among law firms
shares_map <- count(data, region, year, kind) %>% 
  group_by(region, year) %>%
  mutate(share = n / sum(n)) %>% 
  filter(kind == 1) %>% 
  group_by(region) %>% 
  summarise(share = median(share)) %>% 
  right_join(tiles, by = c("region" = "name")) %>% 
  ggplot(aes(x = col, y = -row, fill = share)) +
  geom_raster() +
  geom_text(
    aes(x = col - .4, y = -row + .3, label = code_en), 
    family = "Times New Roman", 
    size = 3,
    hjust = 0) +
  geom_text(
    aes(x = col - .4, y = -row - .3, label = round(100 * share)), 
    family = "Times New Roman", 
    size = 3,
    color = "grey20",
    hjust = 0) +
  scale_fill_steps2(
    name = "Median share of organizations (2016–21)\nNumbers on the map are exact values (%)\nGrey is no data",
    breaks = c(0, 0.45, 0.55),
    labels = scales::percent,
    low = "#c2a5cf", mid = "#f7f7f7", high = "#a6dba0",
    midpoint = 0.5, show.limits = TRUE,
    limits = function(x) round(x, 2)) +
  coord_fixed(expand = FALSE) +
  theme_void(base_family = "Times New Roman", base_size = 11) +
  theme(
    legend.position = c(.9, .05),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.title.position = "top"
  )
shares_map  

# Employees calculations
employees_map <- data %>% 
  filter(year >= 2018) %>% 
  replace_na(list(empl = 0)) %>% 
  mutate(empl = replace(empl, empl == 0, 1)) %>% 
  count(region, year, wt = empl) %>% 
  left_join(empl) %>% 
  mutate(share_regional = 100 * n / empl_total) %>% 
  group_by(year) %>% 
  mutate(share_national = 100 * n / sum(n)) %>% 
  group_by(region) %>% 
  summarise(
    share_regional = median(share_regional),
    share_national = median(share_national)
  ) %>% 
  right_join(tiles, by = c("region" = "name")) %>% 
  left_join(courts_by_region) %>% 
  ggplot(aes(x = col, y = -row, fill = share_national)) +
  geom_raster() +
  geom_text(
    aes(x = col - .4, y = -row + .3, label = code_en), 
    family = "Times New Roman", 
    size = 3,
    hjust = 0) +
  geom_text(
    aes(x = col - .4, y = -row - .3, label = n_courts, group = n_courts), 
    family = "Times New Roman", 
    size = 3,
    hjust = 0,
    na.rm = TRUE) +
  geom_point(
    aes(x = col + .25, y = -row - .25, size = share_regional), 
    shape = "circle open",
    na.rm = TRUE) +
  scale_size_binned(
    name = "Share of region's employees", 
    n.breaks = 4) +
  scale_fill_binned(
    name = "Share of country's lawyers", transform = "log10",
    high = "#eff3ff", low = "#2171b5",
    labels = function(x) round(x, digits = 2)) +
  coord_fixed(expand = FALSE) +
  theme_void(base_family = "Times New Roman", base_size = 11) +
  theme(
    legend.position = c(.95, 0),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.title.position = "top",
    plot.caption = element_text(size = 11),
    plot.caption.position = 
  ) +
  labs(caption = "Digits are numbers of district-level courts. Grey is no data")
employees_map

# Timeline of legal companies count
timeline_map <- count(data, region, year) %>% 
  group_by(region) %>% 
  arrange(year) %>% 
  summarise(
    year = year, 
    rel_count = n / first(n),
    direction = factor(case_when(
      abs(first(rel_count) - last(rel_count)) < .05 ~ 0,
      first(n) < last(n) ~ 1,
      first(n) > last(n) ~ -1
    ), levels = -1:1, ordered = TRUE),
    change = (last(rel_count) - first(rel_count)) * 100,
    alpha = .5) %>% 
  right_join(tiles, by = c("region" = "name")) %>% 
  ggplot(aes(x = year, y = rel_count)) + 
  geom_text(
    aes(
      x = 2021,
      y = 1.25, 
      label = ifelse(
        !is.na(change),
        sub("-", "–", sprintf("%+.0f", change), fixed = TRUE),
        ""),
      alpha = alpha),
    family = "Times New Roman",
    size = 4,
    hjust = 1,
    color = "gray45",
    key_glyph = "blank") +
  geom_line(aes(color = direction)) +
  geom_text(
    data = tiles,
    aes(x = 2016, y = 2, label = code_en),
    family = "Times New Roman",
    size = 3,
    hjust = 0,
    vjust = 1) +
  scale_y_continuous(limits = c(NA, 2)) +
  scale_alpha_continuous(
    name = NULL,
    labels = c("Numbers show change in percent")
  ) +
  scale_color_manual(
    name = "Overall change, 2021 to 2016",
    values = c("#d7191c", "gray45", "#1a9850"),
    labels = c("Decline (–5% and more)", "Stability (±5%)", "Growth (+5% and more)"),
    na.translate = FALSE) +
  facet_grid(rows = vars(row), cols = vars(col)) +
  coord_fixed(expand = FALSE) +
  guides(color = guide_legend(ncol = 2, order = 1), alpha = guide_legend(order = 2)) +
  theme_void(base_family = "Times New Roman", base_size = 11) +
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
timeline_map
