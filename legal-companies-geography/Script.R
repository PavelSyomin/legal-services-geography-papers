library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# Load raw data
data <- read_csv("../../ru-smb-companies/legal/panel.csv")
tiles <- read_csv("russia-tiles.csv")

# Common preparations
data <- data %>% 
  filter(year < 2022) %>% 
  replace_na(list(employees_count = 0)) %>% 
  mutate(
    employees_count = replace(
      employees_count, employees_count == 0, 1),
    kind = replace(kind, kind == 3, 2)
    ) %>% 
  select(tin, kind, category, region, year, empl = employees_count)

# Count of legal firms by region and shares of companies
shares <- count(data, region, year, kind) %>% 
  group_by(region, year) %>%
  mutate(share = n / sum(n)) %>% 
  filter(kind == 1) %>% 
  group_by(region) %>% 
  summarise(share = median(share))

counts_map <- count(data, region, year) %>% 
  group_by(region) %>% 
  summarise(n = median(n)) %>% 
  left_join(shares) %>% 
  right_join(tiles, by = c("region" = "name")) %>% 
  ggplot(aes(x = col, y = -row, fill = n)) +
  geom_raster() +
  geom_point(
    aes(x = col + .25, y = -row - .25, size = share), 
    shape = "circle open",
    na.rm = TRUE) +
  geom_text(
    aes(x = col - .4, y = -row + .3, label = code_en), 
    family = "Times New Roman", 
    size = 3,
    hjust = 0) +
  scale_size_binned(name = "Share of organisations", n.breaks = 6) +
  scale_fill_binned(
    name = "Count of legal firms", transform = "log10",
    high = "#eff3ff", low = "#2171b5") +
  coord_fixed(expand = FALSE) +
  theme_void(base_family = "Times New Roman", base_size = 11) +
  theme(
    legend.position = c(1, .05),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.title.position = "top"
  )
counts_map

