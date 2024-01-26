library(dplyr)
library(ggplot2)
library(readr)
library(scatterpie)
library(sf)
library(tidyr)

data_path <- "../ru-smb-companies/group_A/panel.csv"
data <- read_csv(data_path)

total <- data %>% distinct(tin) %>% nrow()

sample_row <- data %>%
  filter(year == 2021, kind == 1) %>% 
  slice(1000:1001)
sample_table <- data.frame(
  var = colnames(sample_row),
  value = as.character(sample_row[1, ])
) %>% replace_na(list(value = "—"))

regions_boundaries <- st_read("ru.geojson")
ru_crs <- st_crs("+proj=aea +lat_0=0 +lon_0=100 +lat_1=68 +lat_2=44 +x_0=0 +y_0=0 +ellps=krass +towgs84=28,-130,-95,0,0,0,0 +units=m +no_defs")

plot_1 <- data %>% 
  filter(
    year == 2021,
    kind == 1, 
    substr(activity_code_main, 1, 2) == "01") %>% 
  mutate(activity = case_when(
    substr(activity_code_main, 1, 4) %in% c("01.1", "01.2", "01.3") ~ "Растениеводство",
    substr(activity_code_main, 1, 4) == "01.4" ~ "Животноводство")) %>% 
  drop_na(activity) %>% 
  count(lat, lon, activity) %>% 
  drop_na(lat, lon) %>% 
  group_by(lat, lon) %>% 
  arrange(-n) %>% 
  slice(1) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  ggplot() +
  geom_sf(data = st_combine(regions_boundaries)) + 
  geom_sf(aes(color = activity), size = .1) +
  geom_sf(data = regions_boundaries, size = .1, fill = NA) + 
  coord_sf(crs = ru_crs) +
  scale_color_discrete(name = "Преобладающий вид деятельности") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  theme_bw(base_size = 12, base_family = "Times New Roman") +
  theme(legend.position = "bottom", legend.direction = "horizontal")
plot_1  

