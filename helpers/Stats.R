library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

labels <- data.frame(
  ru = c(
    "Исходные данные",
    "Промежуточные файлы",
    "Итоговые таблицы",
    "Панельное представление",
    "Группы ОКВЭД",
    "Все",
    "Все, кроме O, P, U",
    "Коэффициент корреляции Спирмена",
    "Число субъектов России"
  ),
  en = c(
    "Source data",
    "Intermediary files",
    "Resulting tables",
    "Panel view",
    "Activity groups",
    "All",
    "All except for O, P, U",
    "Spearman's rho",
    "Number of regions"
  )
)
labels <- labels[[LOCALE]]

# Counts
rsmp_data <- read_csv("../ru-smb-companies/group_A/smb.csv")
counts <- rsmp_data %>% 
  mutate(kind = replace(kind, kind == 3, 2)) %>% 
  distinct(tin, .keep_all = TRUE) %>% 
  count(kind) %>% 
  pull(n)

# Validation
val_stats <- read_csv("assets/validation-stats.csv")

corr_by_year_all <- val_stats %>% 
  group_by(year) %>% 
  summarise(cor = cor(count_reestr, count_rosstat, method = "spearman"))
corr_by_year_filtered <- val_stats %>% 
  filter(group != "O", group != "P", group != "U") %>% 
  group_by(year) %>% 
  summarise(cor = cor(count_reestr, count_rosstat, method = "spearman"))
corr_by_year <- left_join(
  corr_by_year_all,
  corr_by_year_filtered,
  by = "year",
  suffix = c("_all", "_filtered")
)

corr_by_region_all <- val_stats %>% 
  group_by(region) %>% 
  summarise(cor = cor(count_reestr, count_rosstat, method = "spearman")) %>% 
  arrange(cor)
corr_by_region_filtered <- val_stats %>% 
  filter(group != "O", group != "P", group != "U") %>% 
  group_by(region) %>% 
  summarise(cor = cor(count_reestr, count_rosstat, method = "spearman")) %>% 
  arrange(cor)
corr_by_region <- left_join(
  corr_by_region_all, corr_by_region_filtered,
  by = "region", suffix = c("_all", "_filtered")) %>% 
  pivot_longer(-region, names_to = "option", values_to = "cor")
corr_by_region_plot <- ggplot(corr_by_region, aes(x = cor, color = option)) +
  geom_freqpoly(binwidth = .05) +
  scale_color_discrete(name = labels[5], labels = c(labels[6], labels[7])) +
  labs(x = labels[8], y = labels[9]) +
  theme_bw(base_size = 12, base_family = "Times New Roman")
corr_by_region_plot

corr_by_group <- val_stats %>% 
  group_by(group) %>% 
  summarise(cor = cor(count_reestr, count_rosstat, method = "spearman")) %>% 
  arrange(cor)
