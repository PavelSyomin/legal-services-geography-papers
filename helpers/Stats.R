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
rsmp_raw_data_path <- "../tax-service-opendata/rsmp/xml"

raw_files <- list.files(rsmp_raw_data_path, pattern = "*.zip", full.names = TRUE)
raw_files_count <- length(raw_files)
raw_files_count

raw_files_size_gb <- sum(file.size(raw_files)) / 2 ** 30
raw_files_size_gb

raw_files_uncompressed_size_tb <- sum(sapply(
  raw_files,
  function(path) {
    sum(unzip(path, list = TRUE)$Length)
})) / 2 ** 40
raw_files_uncompressed_size_tb

# Algorithm execution stats for group A demonstration
rsmp_raw_data_path <- "../tax-service-opendata/rsmp/xml"
revexp_raw_data_path <- "../tax-service-opendata/revexp/xml"
sshr_raw_data_path <- "../tax-service-opendata/sshr/xml"
rsmp_source_files <- list.files(rsmp_raw_data_path, pattern = "*.zip", full.names = TRUE)
revexp_source_files <- list.files(revexp_raw_data_path, pattern = "*.zip", full.names = TRUE)
sshr_source_files <- list.files(sshr_raw_data_path, pattern = "*.zip", full.names = TRUE)
rsmp_source_files_size <- sum(file.size(rsmp_source_files))
revexp_source_files_size <- sum(file.size(revexp_source_files))
sshr_source_files_size <- sum(file.size(sshr_source_files))

rsmp_intermediate_data_path <- "../tax-service-opendata/rsmp/reestr_group_A/"
revexp_intermediate_data_path <- "../tax-service-opendata/revexp/csv"
sshr_intermediate_data_path <- "../tax-service-opendata/sshr/csv"
rsmp_intermediate_data_files <- list.files(rsmp_intermediate_data_path, pattern = "^data-", full.names = TRUE)
revexp_intermediate_data_files <- list.files(revexp_intermediate_data_path, pattern = "^data-", full.names = TRUE)
sshr_intermediate_data_files <- list.files(sshr_intermediate_data_path, pattern = "^data-", full.names = TRUE)
rsmp_intermediate_data_files_size <- sum(file.size(rsmp_intermediate_data_files))
revexp_intermediate_data_files_size <- sum(file.size(revexp_intermediate_data_files))
sshr_intermediate_data_files_size <- sum(file.size(sshr_intermediate_data_files))

rsmp_out_file <- "../tax-service-opendata/rsmp/csv/data_product.csv"
revexp_out_file <- "../tax-service-opendata/revexp/csv/group_A.csv"
sshr_out_file <- "../tax-service-opendata/sshr/csv/group_A.csv"
rsmp_out_file_size <- file.size(rsmp_out_file)
revexp_out_file_size <- file.size(revexp_out_file)
sshr_out_file_size <- file.size(sshr_out_file)

rsmp_panel_file <- "../tax-service-opendata/rsmp/reestr_group_A/panel.csv"
rsmp_panel_file_size <- file.size(rsmp_panel_file)

sizes_funnel <- data.frame(
  rsmp = c(rsmp_source_files_size, rsmp_intermediate_data_files_size, rsmp_out_file_size, rsmp_panel_file_size),
  revexp = c(revexp_source_files_size, revexp_intermediate_data_files_size, revexp_out_file_size, NA),
  sshr = c(sshr_source_files_size, sshr_intermediate_data_files_size, sshr_out_file_size, NA),
  row.names = c(labels[1], labels[2], labels[3], labels[4])
)
sizes_funnel$total <- rowSums(sizes_funnel, na.rm = TRUE)

rsmp_source_obs_count <- sum(sapply(
  rsmp_source_files,
  function(path) {
    length(unzip(path, list = TRUE)$Name) * 900
  }))
rsmp_intermediate_obs_count <- sum(
  sapply(
    rsmp_intermediate_data_files,
    function(path) {
      as.numeric(system(sprintf("cat %s | wc -l", path), intern = TRUE)) - 1
    }
  )
)
revexp_intermediate_obs_count <- sum(sapply(
  revexp_intermediate_data_files,
  function(path) {
    as.numeric(system(sprintf("cat %s | wc -l", path), intern = TRUE)) - 1
  }))
sshr_intermediate_obs_count <- sum(sapply(
  sshr_intermediate_data_files,
  function(path) {
    as.numeric(system(sprintf("cat %s | wc -l", path), intern = TRUE)) - 1
  }))

rsmp_out_obs_count <- as.numeric(system(sprintf("cat %s | wc -l", rsmp_out_file), intern = TRUE)) - 1
revexp_out_obs_count <- as.numeric(system(sprintf("cat %s | wc -l", revexp_out_file), intern = TRUE)) - 1
sshr_out_obs_count <- as.numeric(system(sprintf("cat %s | wc -l", sshr_out_file), intern = TRUE)) - 1

rsmp_panel_file <- "../tax-service-opendata/rsmp/reestr_group_A/panel.csv"
rsmp_panel_obs_count <- as.numeric(system(sprintf("cat %s | wc -l", rsmp_panel_file), intern = TRUE)) - 1

counts_funnel <- data.frame(
  rsmp = c(rsmp_source_obs_count, rsmp_intermediate_obs_count, rsmp_out_obs_count, rsmp_panel_obs_count),
  revexp = c(revexp_intermediate_obs_count, revexp_intermediate_obs_count, revexp_out_obs_count, NA),
  sshr = c(sshr_intermediate_obs_count, sshr_intermediate_obs_count, sshr_out_obs_count, NA),
  row.names = c(labels[1], labels[2], labels[3], labels[4])
)

out_data <- read_csv(rsmp_out_file)
out_org_count <- table(out_data$kind)["1"]
out_ind_count <- table(out_data$kind)["2"]

# Validation
val_stats <- read_csv("../tax-service-opendata/Stats_for_validation.csv")

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

