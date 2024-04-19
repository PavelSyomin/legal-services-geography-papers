library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# Load raw data
courts <- read_csv("courts.csv")
data <- read_csv("../../ru-smb-companies/legal/panel.csv")
empl <- read_csv("empl.csv")
tiles <- read_csv("russia-tiles.csv")

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

# Count of legal firms by region
counts_map <- count(data, region, year) %>% 
  group_by(region) %>% 
  summarise(n = median(n)) %>% 
  left_join(shares) %>% 
  right_join(tiles, by = c("region" = "name")) %>% 
  ggplot(aes(x = col, y = -row, fill = n)) +
  geom_raster() +
  geom_text(
    aes(x = col - .4, y = -row + .3, label = code_en), 
    family = "Times New Roman", 
    size = 3,
    hjust = 0) +
  scale_fill_binned(
    name = "Count of legal firms", transform = "log10",
    high = "#eff3ff", low = "#2171b5") +
  coord_fixed(expand = FALSE) +
  theme_void(base_family = "Times New Roman", base_size = 11) +
  theme(
    legend.position = c(1, .05),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.title.position = "top",
    plot.caption = element_text(size = 11)
  ) +
  labs(caption = "Grey is no data")
counts_map

# Shares of companies among legal firms
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
  scale_fill_binned(
    name = "Share of organizations", transform = "log10",
    n.breaks = 6,
    high = "#eff3ff", low = "#2171b5") +
  coord_fixed(expand = FALSE) +
  theme_void(base_family = "Times New Roman", base_size = 11) +
  theme(
    legend.position = c(1, .05),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.title.position = "top",
    plot.caption = element_text(size = 11)
  ) +
  labs(caption = "Grey is no data")
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
    hjust = 0) +
  geom_point(
    aes(x = col + .25, y = -row - .25, size = share_regional), 
    shape = "circle open",
    na.rm = TRUE) +
  scale_size_binned(
    name = "Share of region's employees", 
    n.breaks = 4) +
  scale_fill_binned(
    name = "Share of country's legal workers", transform = "log10",
    high = "#eff3ff", low = "#2171b5",
    labels = function(x) round(x, digits = 2)) +
  coord_fixed(expand = FALSE) +
  theme_void(base_family = "Times New Roman", base_size = 11) +
  theme(
    legend.position = c(.95, .05),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.title.position = "top",
    plot.caption = element_text(size = 11)
  ) +
  labs(caption = "Digits are the numbers of mutli-regional courts. Grey is no data")
employees_map

# Timeline of legal companies count
count(data, region, year) %>% 
  group_by(region) %>% 
  arrange(year) %>% 
  summarise(
    year = year, 
    rel_count = n / first(n),
    direction = factor(case_when(
      abs(first(rel_count) - last(rel_count)) < .05 ~ 0,
      first(n) < last(n) ~ 1,
      first(n) > last(n) ~ -1
    ), levels = -1:1, ordered = TRUE)) %>% 
  right_join(tiles, by = c("region" = "name")) %>% 
  ggplot(aes(x = year, y = rel_count)) + 
  geom_line(aes(color = direction)) +
  geom_text(
    aes(x = 2016, y = 3, label = code_en),
    family = "Times New Roman",
    size = 3,
    hjust = 0) +
  scale_color_manual(
    name = "Overall change, 2021 to 2016",
    values = c("#d7191c", "gray45", "#1a9850"),
    labels = c("Decline (–10% and more)", "Stability (±10%)", "Growth (+10% and more)", "No data"),
    na.value = "gray30") +
  facet_grid(rows = vars(row), cols = vars(col)) +
  coord_fixed(expand = FALSE) +
  guides(color = guide_legend(ncol = 2)) +
  theme_void(base_family = "Times New Roman", base_size = 11) +
  theme(
    aspect.ratio = 1,
    legend.position = c(1, .05),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.title.position = "top",
    plot.caption = element_text(size = 11),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

