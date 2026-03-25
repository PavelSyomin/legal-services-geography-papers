library(dplyr)
library(fixest)
library(ggplot2)
library(here)
library(readr)
library(stringr)
library(tidyr)
library(zoo)

# Common data
data <- read_delim(
  "assets/sme2026.csv", 
  delim = ";",
  col_types = cols(municipality_code = "c")
)
courts <- read_csv(here("assets/courts.csv"))
cities_courts <- courts %>% 
  rename(settlement = city) %>% 
  group_by(region, settlement) %>% 
  summarise(
    has_ordinary_court = any(branch == "ordinary"),
    has_commercial_court = any(branch == "commercial"),
    .groups = "drop"
  )

# Basic spec: just cities as is
cities_base <- read_csv(
  "assets/cities.csv",
  col_types = cols("oktmo" = "c")
)
cities_additional <- read_csv(
  "assets/cities_additional.csv",
  col_types = cols("oktmo" = "c")
)
cities_ppl = rbind(cities_base, cities_additional) %>% 
  select(settlement = city, municipality_code = oktmo, population)

cpanel <- data %>%
  filter(
    settlement_type == "г", 
    region != "Москва", 
    region != "Санкт-Петербург"
  ) %>%
  distinct(tax_number, year, .keep_all = TRUE) %>% 
  group_by(region, settlement, municipality_code, year) %>%
  summarise(count = n(), .groups = "drop") %>% 
  left_join(cities_ppl) %>% 
  left_join(cities_courts) %>% 
  replace_na(list(
    has_ordinary_court = FALSE,
    has_commercial_court = FALSE
  )) %>%
  drop_na() %>% 
  mutate(
    post = ifelse(year >= 2020, 1, 0),
    treat = as.numeric(has_ordinary_court),
    ln_count = log(count + 1),
    city = paste(region, settlement, sep = ", "),
    city_size = cut(
      population,
      breaks = c(0, 5e4, 1e5, 2.5e5, 5e5, 1e6, 1e8),
      labels = c(
        "small", 
        "medium", 
        "big",
        "large", 
        "largest",
        "millionare"
      ),
      ordered_result = TRUE
    )
  ) %>% 
  group_by(region, settlement) %>% 
  mutate(n_years = n_distinct(year)) %>% 
  ungroup() %>% 
  filter(n_years == 11, city_size >= "largest")

count(cpanel, treat)

model <- feols(
  ln_count ~ treat:post | city + year, 
  data = cpanel,
  cluster = ~city
)
summary(model)

es <- feols(
  ln_count ~ i(year, treat, ref = 2018) | city + year, 
  data = cpanel, 
  cluster = ~city
)
summary(es)
iplot(
  es, 
  main = "Event Study: Влияние на число юрфирм (log)",
  xlab = "Год",
  sub = "Доверительные интервалы 95%. Базовый год: 2018"
)

# Robustness check with agglomerations
agglomerations <- read_csv("agglomerations.csv", col_types = cols("oktmo_stable_center" = "c", "oktmo_stable_part" = "c")) %>% 
  mutate(
    oktmo_stable_center = str_pad(oktmo_stable_center, 8, "left", "0"),
    oktmo_stable_part = str_pad(oktmo_stable_part, 8, "left", "0")
  ) %>% 
  distinct(oktmo_stable_part, .keep_all = TRUE)
population <- read_csv2_chunked(
  "assets/data_Y48112027_112_v20250918.csv",
  callback = DataFrameCallback$new(
    function(part, idx) {
      part %>% 
        filter(
          year >= 2016,
          oktmo_stable != "CD"
        ) %>% 
        select(
          oktmo,
          oktmo_stable,
          municipality,
          year,
          mest,
          indicator_period,
          indicator_value
        )
    }
  )
)

# Prepare a mapping of data and BDMO (settlement to municipality)
bdmo_oktmo <- population %>% 
  filter(year == 2021) %>%  # approx. matches municipality code year
  distinct(municipality, oktmo, oktmo_stable) %>% 
  rename(oktmo_2021 = oktmo)

# Incomplete list to filter out (no data in BDMO)
closed_cities <- c(
  "Межгорье",
  "Циолковский",
  "Вилючинск",
  "Железногорск",
  "Зеленогорск",
  "Лесной",
  "Мирный",
  "Радужный",
  "Новоуральск"
)

cities_oktmo <- data %>% 
  filter(
    settlement_type == "г", 
    !(settlement %in% closed_cities)
  ) %>% 
  distinct(municipality_code, .keep_all = TRUE) %>%
  mutate(
    oktmo_2021 = case_when(
      str_length(municipality_code) == 10 ~ str_pad(municipality_code, 11, "left", "0"),
      str_length(municipality_code) == 7 ~ str_pad(municipality_code, 8, "left", "0"),
      .default = municipality_code
    ),
    oktmo_2021 = substr(oktmo_2021, 1, 8),
    # BDMO data points at area-level municipalities with OKTMOs ending with zeros
    oktmo_2021 = if_else(
      substr(oktmo_2021, 6, 8) != "000",
      paste0(substr(oktmo_2021, 1, 5), "000"),
      oktmo_2021
    )
  ) %>% 
  select(region, settlement, oktmo_2021)

automatic_oktmo_mapping <- cities_oktmo %>% 
  left_join(bdmo_oktmo)

manual_mapping_addon <- tribble(
  ~region, ~settlement, ~oktmo_2021, ~municipality, ~oktmo_stable,
  # replace region-level code with municipality-level one
  "Севастополь", "Севастополь", "67000000", "Ленинский муниципальный округ
", "67312000",
  # oktmo_2021 in data == oktmo_stable in BDMO
  "Брянская область", "Стародуб", "15501000", "Город Стародуб
", "15501000",
  "Брянская область", "Жуковка", "15502000", "Жуковский муниципальный район
", "15502000"
)

mismatch <- filter(automatic_oktmo_mapping, is.na(municipality))
# There should be 6 rows: 
# - Moscow, Saint Petersburg, Zelenograd (part of Moscow) - omitted,
# - Sevastopol Starodub, Zhukovka - manually fixed with addon, 
stopifnot(nrow(mismatch) == 6)

oktmo_mapping <- rbind(
  automatic_oktmo_mapping,
  manual_mapping_addon
) %>% 
  left_join(
    agglomerations,
    by = c("oktmo_stable" = "oktmo_stable_part")
  ) %>% 
  mutate(oktmo_stable_aggl = coalesce(oktmo_stable_center, oktmo_stable)) %>% 
  select(
    region,
    settlement,
    oktmo_stable,
    oktmo_stable_aggl
  ) %>% 
  drop_na()

courts_data <- cities_courts %>% 
  left_join(oktmo_mapping) %>% 
  filter(
    !(region == "Калужская область" & settlement == "Киров"),
    !is.na(oktmo_stable)
  ) %>% 
  select(oktmo_stable, has_ordinary_court, has_commercial_court)

# Count law firms per municipality
law_firms_data <- data %>%
  filter(settlement_type == "г") %>%
  distinct(tax_number, year, .keep_all = TRUE) %>%
  rename(empl = employees_count) %>%
  replace_na(list(empl = 0)) %>%
  mutate(
    revenue = replace(revenue, is.na(revenue), 0),
    empl = replace(empl, empl == 0, 1)
  ) %>%
  group_by(region, settlement, year) %>%
  summarise(count = n(), rev = sum(revenue), empl = sum(empl), .groups = "drop") %>% 
  left_join(oktmo_mapping, by = c("region", "settlement")) %>% 
  group_by(oktmo_stable_aggl, year) %>% 
  summarise(across(count:empl, sum)) %>% 
  rename(oktmo_stable = oktmo_stable_aggl)

population_data <- population %>% 
  filter(mest == "Городское население") %>% 
  complete(oktmo, year) %>% 
  group_by(oktmo) %>% 
  arrange(year, .by_group = TRUE) %>% 
  mutate(indicator_value = na.approx(indicator_value, na.rm = F)) %>% 
  fill(oktmo_stable:indicator_period) %>% 
  drop_na() %>% 
  ungroup() %>% 
  left_join(
    distinct(oktmo_mapping, oktmo_stable, oktmo_stable_aggl),
    by = "oktmo_stable"
  ) %>% 
  count(oktmo_stable_aggl, year, wt = indicator_value, name = "population") %>% 
  rename(oktmo_stable = oktmo_stable_aggl)

apanel <- law_firms_data %>% 
  left_join(population_data, by = c("oktmo_stable", "year")) %>% 
  left_join(courts_data) %>% 
  replace_na(list(
    has_ordinary_court = FALSE,
    has_commercial_court = FALSE
  )) %>%
  drop_na(oktmo_stable, year, count) %>% 
  mutate(
    post = ifelse(year >= 2020, 1, 0),
    treat = as.numeric(has_ordinary_court),
    ln_count = log(count + 1),
  ) %>% 
  group_by(oktmo_stable) %>% 
  mutate(
    median_ppl = median(population, na.rm = TRUE),
    n_years = n(),
    city_size = cut(
      median_ppl,
      breaks = c(0, 5e4, 1e5, 2.5e5, 5e5, 1e6, 1e8),
      labels = c(
        "small", 
        "medium", 
        "big",
        "large", 
        "largest",
        "millionare"
      ),
      ordered_result = TRUE
    )
  ) %>% 
  ungroup() %>% 
  filter(
    n_years >= 9,
    city_size >= "large",
    !(oktmo_stable %in% c("40908000", "45000000"))
  )

count(apanel, treat)

model <- feols(
  ln_count ~ treat:post | oktmo_stable + year, 
  data = apanel,
  cluster = ~oktmo_stable
)
summary(model)

es <- feols(
  ln_count ~ i(year, treat, ref = 2018) | oktmo_stable + year, 
  data = apanel, 
  cluster = ~oktmo_stable
)
summary(es)
iplot(
  es, 
  main = "Event Study: Влияние на число юрфирм (log)",
  xlab = "Год",
  sub = "Доверительные интервалы 95%. Базовый год: 2018"
)

# Placebo test
set.seed(42)
random_cities <- sample(unique(apanel$oktmo_stable), 10)
apanel_placebo <- apanel %>%
  mutate(treat_fake = ifelse(oktmo_stable %in% random_cities, 1, 0))

model_placebo <- feols(
  ln_count ~ treat_fake:post | oktmo_stable + year, 
  data = apanel_placebo, 
  cluster = ~oktmo_stable
)
summary(model_placebo)
es_placebo <- feols(
  ln_count ~ i(year, treat_fake, ref = 2017) | oktmo_stable + year, 
  data = apanel_placebo, 
  cluster = ~oktmo_stable
)
summary(es_placebo)
iplot(
  es_placebo, 
  main = "Event Study: Влияние на число юрфирм (log)",
  xlab = "Год",
  sub = "Доверительные интервалы 95%. Базовый год: 2018"
)

# Leave-one-out
treated_cities <- unique(apanel[apanel$treat == 1, ]$oktmo_stable)
loo_results <- do.call(
  rbind,
  lapply(
    treated_cities,
    function(city) {
      tmp <- apanel %>% filter(oktmo_stable != city)
      res <- feols(
        ln_count ~ treat:post | oktmo_stable + year, 
        data = tmp, 
        cluster = ~oktmo_stable
      )
      
      data.frame(
        excluded_city = city,
        estimate = coef(res)["treat:post"],
        se = se(res)["treat:post"],
        p_val = pvalue(res)["treat:post"]
      )
    }
  )
)


ggplot(loo_results, aes(x = reorder(excluded_city, estimate), y = estimate)) +
  geom_pointrange(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Leave-one-out Analysis (Effect of treat:post)",
       subtitle = "Каждая точка — коэффициент БЕЗ указанного города",
       x = "Исключенный город (ОКТМО)", y = "Оценка коэффициента") +
  theme_minimal()


rfsd <- read_csv("assets/rfsd_extracted_agg.csv")

rpanel <- rfsd %>% 
  mutate(
    oktmo = paste0(substr(oktmo, 1, 5), "000")
  ) %>% 
  left_join(bdmo_oktmo, by = c("oktmo" = "oktmo_2021")) %>% 
  filter(!is.na(oktmo_stable)) %>%
  left_join(population_data) %>% 
  left_join(courts_data) %>%
  group_by(oktmo_stable, year) %>% 
  summarise(
    count = sum(count), 
    has_ordinary_court = any(has_ordinary_court, na.rm = T), 
    has_commercial_court = any(has_commercial_court, na.rm = T), 
    population = median(population, na.rm = T)
  ) %>% 
  group_by(oktmo_stable) %>% 
  mutate(
    ln_count = log(count + 1),
    post = ifelse(year >= 2020, 1, 0),
    treat = as.numeric(has_ordinary_court),
    median_ppl = median(population, na.rm = T),
    n_years = n(),
    city_size = cut(
      median_ppl,
      breaks = c(0, 5e4, 1e5, 2.5e5, 5e5, 1e6, 1e8),
      labels = c(
        "small", 
        "medium", 
        "big",
        "large", 
        "largest",
        "millionare"
      ),
      ordered_result = TRUE
    )
  ) %>% 
  ungroup() %>% 
  filter(
    n_years == 14,
    city_size >= "largest",
    !(oktmo_stable %in% c("40908000", "45000000"))
  )

count(rpanel, treat)

es <- feols(
  ln_count ~ i(year, treat, ref = 2011:2015) | oktmo_stable + year, 
  data = rpanel, 
  cluster = ~oktmo_stable
)
summary(es)
iplot(
  es, 
  main = "Event Study: Влияние на число юрфирм (log)",
  xlab = "Год",
  sub = "Доверительные интервалы 95%. Базовый год: 2018"
)

# Time-placebo
rpanel2017 <- filter(rpanel, year <= 2017) %>% 
  mutate(post = if_else(year >= 2014, 1, 0))
es <- feols(
  ln_count ~ i(year, treat, ref = 2013) | oktmo_stable + year, 
  data = rpanel2017, 
  cluster = ~oktmo_stable
)
summary(es)
iplot(
  es, 
  main = "Event Study: Влияние на число юрфирм (log)",
  xlab = "Год",
  sub = "Доверительные интервалы 95%. Базовый год: 2018"
)
