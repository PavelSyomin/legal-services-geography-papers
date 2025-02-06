library(dplyr)
library(ggplot2)
library(here)
library(readr)
library(tidyr)

panel <- read_csv(here("../large-datasets/law-firms/panel.csv"))
empl <- read_csv(here("assets/empl.csv"))
tiles <- read_csv(here("assets/russia-tiles.csv"))

spec <- panel %>% 
  filter(year == 2021) %>% 
  drop_na(region) %>% 
  replace_na(list(employees_count = 0)) %>% 
  mutate(employees_count = replace(employees_count, employees_count == 0, 1)) %>% 
  count(region, wt = employees_count, name = "legal_empl") %>% 
  right_join(empl, by = "region") %>% 
  rename(total_empl = count) %>% 
  mutate(
    concentration = legal_empl / sum(legal_empl, na.rm = TRUE),
    empl_share = total_empl / sum(total_empl, na.rm = TRUE),
    localization = concentration / empl_share
  ) %>% 
  arrange(-concentration) %>% 
  mutate(cum_concentraion = cumsum(concentration)) %>% 
  arrange(-localization) %>% 
  mutate(loc_rank = row_number()) %>% 
  mutate(
    is_national = cum_concentraion <= 0.8,
    is_regional = loc_rank < nrow(.) * .2,
    rank = as.numeric(is_national) * 2 + as.numeric(is_regional),
    rank = factor(rank, labels = c(NA, "Local", "National", "Both"))
  ) %>% 
  select(region, is_national, is_regional, rank)


map <- left_join(spec, tiles, by = c("region" = "name")) %>% 
  ggplot(aes(x = col, y = -row, fill = rank)) +
  geom_raster() +
  geom_text(aes(label = code_en), family = "Times New Roman", size = 3) +
  scale_fill_brewer(
    palette = "Blues",
    na.value = "grey90",
    name = "Legal services importance",
    labels = c("Local", "National", "Both", "No")) +
  coord_equal() +
  guides(fill = guide_legend(title.position = "top")) +
  theme_void(base_family = "Times New Roman") +
  theme(
    legend.position = c(.9, .1),
    legend.justification = c(1, 0),
    legend.direction = "horizontal"
  ) 
map
