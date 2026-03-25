library(dplyr)
library(fixest)
library(ggplot2)
library(here)
library(MatchIt)
library(nanoparquet)
library(readr)
library(stringr)
library(tidyr)
library(zoo)

# Main data
data <- read_delim(
  "assets/sme2026.csv", 
  delim = ";",
  col_types = cols(municipality_code = "c")
)
agglomerations <- read_csv("agglomerations.csv", col_types = cols("oktmo_stable_center" = "c", "oktmo_stable_part" = "c")) %>% 
  mutate(
    oktmo_stable_center = str_pad(oktmo_stable_center, 8, "left", "0"),
    oktmo_stable_part = str_pad(oktmo_stable_part, 8, "left", "0")
  ) %>% 
  distinct(oktmo_stable_part, .keep_all = TRUE)
cities_base <- read_csv(
  "assets/cities.csv",
  col_types = cols("oktmo" = "c")
)
cities_additional <- read_csv(
  "assets/cities_additional.csv",
  col_types = cols("oktmo" = "c")
)
cities <- rbind(cities_base, cities_additional)
courts <- read_csv(here("assets/courts.csv"))
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
area <- read_csv2_chunked(
  "assets/data_Y48006001_112_v20250918.csv", 
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
          indicator_period,
          indicator_value
        )
    }
  )
)

# Misc controls
goods_1 <- vroom("assets/data_Y48201001_112_v20250918.csv")
goods_2 <- vroom("assets/data_Y48401011_112_v20250918.csv")
invest <- vroom("assets/data_section9_112_v20250918.csv")
residential <- vroom("assets/data_Y48008019_112_v20250918.csv")
trade1 <- vroom("assets/data_Y48201003_112_v20250918.csv")
trade2 <- vroom("assets/data_Y48401003_112_v20250918.csv")
wages_1 <- vroom("assets/data_Y48123007_112_v20250918.csv")
wages_2 <- vroom("assets/data_Y48423007_112_v20250918.csv")

migrations <- do.call(
  rbind,
  lapply(
    list.files("assets", pattern = "*.parquet"),
    function(fn) {
      df <- read_parquet(paste("assets", fn, sep = "/"))
      res <- df %>% 
        filter(
          migr == "Миграция — всего",
          grup_2 == "Всего",
          vozr == "Всего"
        ) %>% 
        select(indicator_code, municipality, oktmo_stable, year, indicator_name, indicator_value)
      rm(df)
      gc()
      return(res)
    }
  )
)

# Basic spec: just cities as is
cities_courts <- courts %>% 
  rename(settlement = city) %>% 
  group_by(region, settlement) %>% 
  summarise(
    has_ordinary_court = any(branch == "ordinary"),
    has_commercial_court = any(branch == "commercial")
  )
cities_ppl = select(cities, settlement = city, municipality_code = oktmo, population)

add_months_base <- function(x, n) {
  d <- as.POSIXlt(x)
  d$mon <- d$mon + n
  as.Date(d)
}
q_starts <- seq(as.Date("2016-01-01"), as.Date("2026-10-01"), by = "3 months")

quarters <- tibble(
  quarter_start = q_starts,
  quarter_end   = add_months_base(quarter_start, 3) - 1
) %>%
  mutate(
    year = as.integer(format(quarter_start, "%Y")),
    q    = as.integer(substr(quarters(quarter_start), 2, 2))  # 1–4 from "Q1".."Q4"
  )

counts_list <- lapply(seq_len(nrow(quarters)), function(i) {
  qs <- quarters$quarter_start[i]
  qe <- quarters$quarter_end[i]
  
  res <- data %>%
    filter(
      settlement_type == "г", 
      region != "Москва", 
      region != "Санкт-Петербург"
    ) %>%
    distinct(tax_number, start_date, end_date, .keep_all = TRUE) %>% 
    filter(start_date <= qe, end_date >= qs) %>% 
    group_by(region, settlement, municipality_code) %>%
    summarise(count = n(), .groups = "drop") %>% 
    mutate(year = quarters$year[i], quarter = quarters$q[i])
  
  res
})

qpanel <- bind_rows(counts_list) %>% 
  mutate(
    time_id = paste0(year, "Q", quarter)
  ) %>%
  left_join(cities_ppl) %>% 
  left_join(cities_courts, by = c("region", "settlement")) %>% 
  replace_na(list(
    has_ordinary_court = FALSE,
    has_commercial_court = FALSE
  )) %>%
  drop_na() %>% 
  mutate(
    post_law = ifelse((year >= 2018 & quarter >= 3) | year >= 2019, 1, 0),
    post_act = ifelse((year >= 2019 & quarter >= 4) | year >= 2020, 1, 0),
    treat = as.numeric(has_ordinary_court),
    ln_count = log(count + 1),
    city = paste(region, settlement, sep = ", "),
    size = cut(
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
  filter(n_years == 11, size >= "largest")

model <- feols(
  ln_count ~ treat:post_law | city + year^quarter, 
  data = qpanel,
  cluster = ~city
)
summary(model)

qpanel$rel_q <- (as.numeric(qpanel$year) - 2019) * 4 + qpanel$quarter - 3
es <- feols(
  ln_count ~ i(rel_q, treat, ref = 0) | city + year^quarter, 
  data = qpanel, 
  cluster = ~city
)
summary(es)
iplot(
  es, 
  main = "Event Study: Влияние на число юрфирм (log)",
  xlab = "Год",
  sub = "Доверительные интервалы 95%. Базовый год: 2018"
)

qdata <- data %>%
  filter(
    settlement_type == "г", 
    region != "Москва", 
    region != "Санкт-Петербург"
  ) %>%
  distinct(tax_number, start_date, end_date, .keep_all = TRUE) %>% 
  select(tax_number, start_date, end_date, region, settlement, municipality_code, year) %>% 
  mutate(
    month = purrr::pmap(list(start_date, end_date), \(s, e) seq(s, e, by = "month"))
  ) %>%
  unnest(month) %>%
  mutate(
    quarter = as.numeric(substr(quarters(month), 2, 2)),
    year = as.numeric(format(month, "%Y")),
    time_id = paste0(year, "Q", quarter)
  ) %>%
  group_by(tax_number, time_id) %>% 
  slice_head(n = 1)
panel <- qdata %>% 
  group_by(region, settlement, municipality_code, year, quarter, time_id) %>%
  summarise(count = n(), .groups = "drop") %>% 
  left_join(cities_ppl) %>% 
  left_join(cities_courts, by = c("region", "settlement")) %>% 
  replace_na(list(
    has_ordinary_court = FALSE,
    has_commercial_court = FALSE
  )) %>%
  drop_na() %>% 
  mutate(
    post_law = ifelse((year >= 2018 & quarter >= 3) | year >= 2019, 1, 0),
    post_act = ifelse((year >= 2019 & quarter >= 4) | year >= 2020, 1, 0),
    treat = as.numeric(has_ordinary_court),
    ln_count = log(count + 1),
    #ln_empl = log(empl + 1),
    city = paste(region, settlement, sep = ", "),
    size = cut(
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
  filter(n_years == 11, size >= "largest") %>% 
  filter(
    !(settlement %in% c("Воронеж", "Иваново", "Томск", "Казань", "Пермь", "Калуга"))
  )

count(panel, treat)

model <- feols(
  ln_count ~ treat:post_law | city + year^quarter, 
  data = panel,
  cluster = ~city
)
summary(model)

panel <- panel %>% mutate(relq = (year - 2018) * 4 + quarter - 2)
es <- feols(
  ln_count ~ i(relq, treat, ref = 0) | city + year^quarter, 
  data = panel, 
  cluster = ~city
)
summary(es)
iplot(
  es, 
  main = "Event Study: Влияние на число юрфирм (log)",
  xlab = "Год",
  sub = "Доверительные интервалы 95%. Базовый год: 2018"
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

# Extract necessary BDMO data portions and preprocess them
bdmo_data <- do.call(
  rbind,
  lapply(
    list(area, goods_1, goods_2, invest, migrations, population, trade1, trade2, wages_1, wages_2),
    function(tbl) {
      ind_code <- unique(tbl$indicator_code)[1]
      d <- tbl %>% 
        semi_join(oktmo_mapping, by = "oktmo_stable") %>% 
        filter(year >= 2016)
      if ("mest" %in% names(d)) d <- filter(d, mest == "Городское население")
      if ("schil" %in% names(d)) d <- filter(d, schil == "Весь жилищный фонд")
      if ("istinv" %in% names(d)) {
        d <- d %>% 
          filter(
            istinv %in% c("Всего", "Средства местного бюджета"),
            okved2 == "Всего по обследуемым видам экономической деятельности",
            indicator_code %in% c("Y48009001", "Y48109001")
          ) %>% 
          count(municipality, oktmo_stable, year, indicator_name, wt = indicator_value, name = "indicator_value")
      }
      if (ind_code == "Y48201001") {
        d <- filter(
          d, 
          okved != "Всего",
          indicator_period == "Январь-декабрь"
        ) %>% 
          count(municipality, oktmo_stable, year, indicator_name, wt = indicator_value, name = "indicator_value")
      }
      if (ind_code == "Y48401001") {
        d <- filter(
          d, 
          okved2 == "Всего по обследуемым видам экономической деятельности",
          indicator_period == "Январь-декабрь"
        )
      }
      if (ind_code == "Y48123007") {
        d <- filter(d, okved == "Всего", indicator_period == "Январь-декабрь")
      }
      if (ind_code == "Y48201003") {
        d <- filter(
          d, 
          okved != "Всего",
          indicator_period == "Январь-декабрь"
        ) %>% 
          count(municipality, oktmo_stable, year, indicator_name, wt = indicator_value, name = "indicator_value")
      }
      if (ind_code == "Y48401003") {
        d <- filter(
          d,
          okved2 == "Всего по обследуемым видам экономической деятельности",
          indicator_period == "Январь-декабрь"
        )
      }
      if (ind_code == "Y48423007") {
        d <- filter(
          d, 
          okved2 == "Всего по обследуемым видам экономической деятельности", 
          indicator_period == "Январь-декабрь")
      }
      d %>% 
        select(municipality, oktmo_stable, year, indicator_name, indicator_value)
    }
  )
) %>% 
  distinct(
    municipality, oktmo_stable, year, indicator_name, .keep_all = TRUE
  ) %>% 
  mutate(
    indicator_name = factor(
      indicator_name,
      labels = c("invest", "migrations", "trade", "area", "goods", "population", "wages", "wages")
    )
  ) %>% 
  pivot_wider(
    id_cols = municipality:year,
    names_from = indicator_name,
    values_from = indicator_value
  ) %>% 
  filter(year <= 2023) %>% 
  group_by(oktmo_stable, year) %>% 
  summarise(
    area = sum(area, na.rm = TRUE),
    goods = sum(goods, na.rm = TRUE),
    migrations = sum(migrations, na.rm = TRUE),
    invest = sum(invest, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    trade = sum(trade, na.rm = TRUE),
    wages = mean(wages, na.rm = TRUE)
  )

courts_data <- courts %>% 
  rename(settlement = city) %>% 
  group_by(settlement) %>% 
  summarise(
    has_ordinary_court = any(branch == "ordinary"),
    has_commercial_court = any(branch == "commercial")
  ) %>% 
  left_join(oktmo_mapping) %>% 
  filter(
    !(region == "Калужская область" & settlement == "Киров"),
    !is.na(oktmo_stable)
  ) %>% 
  select(oktmo_stable, has_ordinary_court, has_commercial_court)

# Count law firms per municipality
law_firms_data <- data %>%
  filter(settlement_type == "г", year >= 2016, year <= 2025) %>%
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
  count(oktmo_stable, year, wt = indicator_value, name = "population")

area_data <- area %>% 
  count(oktmo_stable, year, wt = indicator_value, name = "area")

regional_centers <- cities %>% 
  filter(capital_marker == 2) %>% 
  mutate(
    oktmo_2021 = case_when(
      str_length(oktmo) == 10 ~ str_pad(oktmo, 11, "left", "0"),
      str_length(oktmo) == 7 ~ str_pad(oktmo, 8, "left", "0"),
      .default = oktmo
    ),
    oktmo_2021 = substr(oktmo_2021, 1, 8),
    # BDMO data points at area-level municipalities with OKTMOs ending with zeros
    oktmo_2021 = if_else(
      substr(oktmo_2021, 6, 8) != "000",
      paste0(substr(oktmo_2021, 1, 5), "000"),
      oktmo_2021
    )
  ) %>% 
  left_join(bdmo_oktmo) %>% 
  pull(oktmo_stable)

panel <- law_firms_data %>% 
  left_join(population_data, by = c("oktmo_stable", "year")) %>% 
  left_join(courts_data) %>% 
  replace_na(list(
    has_ordinary_court = FALSE,
    has_commercial_court = FALSE
  )) %>%
  drop_na() %>% 
  mutate(
    post = ifelse(year >= 2020, 1, 0),
    treat = as.numeric(has_ordinary_court),
    ln_count = log(count + 1),
    ln_empl = log(empl + 1),
    ln_population = log(population + 1),
    is_regional_center = oktmo_stable %in% regional_centers
  ) %>% 
  group_by(oktmo_stable) %>% 
  mutate(
    median_ppl = median(population),
    n_years = n(),
    size = cut(
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
    n_years >= 9
  )

panel %>% 
  filter(is_regional_center == T) %>% 
  distinct(oktmo_stable) %>% 
  left_join(oktmo_mapping) %>% 
  right_join(data.frame(oktmo_stable = regional_centers)) %>% 
  filter(is.na(region))

panel %>% filter(treat == 1) %>% count(size)
  

model <- feols(
  ln_count ~ treat:post | oktmo_stable + year, 
  data = panel,
  cluster = ~oktmo_stable
)
summary(model)

es <- feols(
  ln_count ~ i(year, treat, ref = 2018) | oktmo_stable + year, 
  data = panel %>% filter(size >= "largest"), 
  cluster = ~oktmo_stable
)
summary(es)
iplot(
  es, 
  main = "Event Study: Влияние на число юрфирм (log)",
  xlab = "Год",
  sub = "Доверительные интервалы 95%. Базовый год: 2018"
)

# Auto-matching
panel_2017_count_changes <- panel %>% 
  filter(year <= 2017) %>% 
  select(oktmo_stable, year, count) %>% 
  pivot_wider(
    names_from = year,
    values_from = count,
    names_prefix = "count_"
  ) %>% 
  mutate(
    change_2016_2017 = count_2017 - count_2016,
  )

panel_2017 <- panel %>% 
  filter(year == 2017) %>%
  left_join(panel_2017_count_changes) %>% 
  drop_na()

max_ppl_change <- matched_panel %>% 
  group_by(oktmo_stable, treat) %>% 
  arrange(year, .by_group = T) %>% 
  summarise(c = last(population) - first(population)) %>% 
  arrange(-abs(c)) %>% 
  group_by(treat) %>% 
  slice_head(n = 5) %>% 
  pull(oktmo_stable)

m.out <- matchit(
  treat ~ count_2016 + count_2017 + change_2016_2017 + ln_population, 
  data = panel_2017, 
  method = "nearest", 
  distance = "glm",
  exact = c("has_commercial_court", "size", "is_regional_center"),
  ratio = 3
)
matched_data_2018 <- match.data(m.out)
matched_ids <- unique(matched_data_2018$oktmo_stable)
matched_panel <- panel %>% 
  filter(oktmo_stable %in% matched_ids)

matched_panel %>% 
  filter(!(oktmo_stable %in% max_ppl_change)) %>% 
  group_by(year, treat) %>% 
  summarise(ppl = median(ln_population)) %>% 
  ggplot(aes(x = year, y = ppl, col = factor(treat), group = treat)) +
  geom_line()

matched_panel %>% 
  ggplot(aes(x = year, y = ln_population, col = factor(treat), group = oktmo_stable)) +
  geom_line()

model_matched <- feols(
  ln_count ~ treat:post | oktmo_stable + year, 
  data = matched_panel,
  cluster = ~oktmo_stable
)
summary(model_matched)

model_matched <- feols(
  ln_count ~ treat:post | oktmo_stable + year, 
  data = matched_panel %>% filter(!(oktmo_stable %in% max_ppl_change)), 
  cluster = ~oktmo_stable
)
summary(model_matched)

es_matched <- feols(
  ln_count ~ i(year, treat, ref = 2018) | oktmo_stable + year, 
  data = matched_panel,
  cluster = ~oktmo_stable
)
summary(es_matched)

es_matched <- feols(
  ln_count ~ i(year, treat, ref = 2018) | oktmo_stable + year, 
  data = matched_panel %>% filter(!(oktmo_stable %in% max_ppl_change)),
  cluster = ~oktmo_stable
)
summary(es_matched)

# 2. Визуализируем
iplot(es_matched, 
      main = "Event Study (Matched Sample)",
      xlab = "Год", 
      ylab = "Эффект политики")

set.seed(42)
random_cities <- sample(unique(panel_reduced$oktmo_stable), 10)
panel_placebo_group <- panel %>%
  mutate(treat_fake = ifelse(oktmo_stable %in% random_cities, 1, 0))

model_placebo_group <- feols(
  ln_count ~ treat_fake:post | oktmo_stable + year, 
  data = panel_placebo_group, 
  cluster = ~oktmo_stable
)
summary(model_placebo_group) # Ожидаем незначимый коэффициент

# Leave-one-out
# 1. Определяем список ОКТМО, которые входят в Treatment группу
treated_cities <- matched_panel %>% 
  filter(treat == 1) %>% 
  distinct(oktmo_stable) %>% 
  pull()

# 2. Создаем контейнер для результатов
loo_results <- data.frame()

# 3. Запускаем цикл (исключаем по одному городу из 10)
for (city_to_exclude in treated_cities) {
  
  # Фильтруем данные: убираем только ОДИН город из Тритмента, 
  # но оставляем ВСЮ контрольную группу (где treat == 0)
  temp_data <- matched_panel %>% 
    filter(oktmo_stable != city_to_exclude)
  
  # Оцениваем модель (используем вашу спецификацию)
  # Ограничение по населению (> 5e5), как вы писали, где результат значим
  res <- feols(
    ln_count ~ treat:post | oktmo_stable + year, 
    data = temp_data, 
    cluster = ~oktmo_stable
  )
  
  # Сохраняем коэффициент, ошибку и название исключенного города
  loo_results <- rbind(loo_results, data.frame(
    excluded_city = as.character(city_to_exclude),
    estimate = coef(res)["treat:post"],
    se = se(res)["treat:post"],
    p_val = pvalue(res)["treat:post"]
  ))
}

# 4. Визуализация (Forest Plot)
ggplot(loo_results, aes(x = reorder(excluded_city, estimate), y = estimate)) +
  geom_pointrange(aes(ymin = estimate - 1.96*se, ymax = estimate + 1.96*se)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Leave-one-out Analysis (Effect of treat:post)",
       subtitle = "Каждая точка — коэффициент БЕЗ указанного города",
       x = "Исключенный город (ОКТМО)", y = "Оценка коэффициента") +
  theme_minimal()


# Извлекаем данные о балансе
summ_data <- as.data.frame(summary(m.out)$sum.all[, "Std. Mean Diff.", drop = FALSE])
summ_matched <- as.data.frame(summary(m.out)$sum.matched[, "Std. Mean Diff.", drop = FALSE])

# Собираем таблицу для графика
plot_df <- data.frame(
  Variable = rownames(summ_data),
  Before = abs(summ_data[[1]]),
  After = abs(summ_matched[[1]])
) %>% 
  pivot_longer(-Variable, names_to = "Sample", values_to = "SMD")

# Строим Love Plot
ggplot(plot_df, aes(x = SMD, y = Variable, color = Sample)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.1, linetype = "dashed", color = "darkred") +
  theme_bw() +
  labs(title = "Love Plot: Баланс ковариат", x = "Absolute Std. Mean Difference", y = "")

law_firms_data_matched_q <- data %>%
  left_join(oktmo_mapping, by = c("region", "settlement")) %>% 
  filter(settlement_type == "г", year >= 2016, year <= 2023, oktmo_stable %in% matched_ids) %>%
  distinct(tax_number, start_date, end_date, .keep_all = TRUE) %>%
  rename(empl = employees_count) %>%
  replace_na(list(empl = 0)) %>%
  mutate(
    revenue = replace(revenue, is.na(revenue), 0),
    empl = replace(empl, empl == 0, 1)
  ) %>%
  # Генерируем последовательность месяцев между start и end для каждой записи
  rowwise() %>%
  mutate(month = list(seq(start_date, end_date, by = "month"))) %>%
  unnest(month) %>%
  # Переходим к кварталам
  mutate(
    quarter = quarters(month),
    year = format(month, "%Y"),
    time_id = paste0(year, quarter)
  ) %>%
  # Группируем, чтобы схлопнуть месяцы в кварталы (берем среднее или сумму)
  group_by(tax_number, time_id) %>% 
  slice_head(n = 1) %>% 
  group_by(region, settlement, year, quarter, time_id) %>% 
  summarise(count = n(), rev = sum(revenue), empl = sum(empl), .groups = "drop") %>% 
  left_join(oktmo_mapping, by = c("region", "settlement")) %>% 
  group_by(oktmo_stable, year, quarter, time_id) %>% 
  summarise(across(count:empl, sum))

panel_matched_q <- law_firms_data_matched_q %>% 
  #left_join(bdmo_data, by = c("oktmo_stable", "year")) %>% 
  left_join(courts_data) %>% 
  left_join(agglomerations, by = c("oktmo_stable" = "oktmo_stable_part")) %>% 
  mutate(oktmo_stable = coalesce(oktmo_stable_center, oktmo_stable)) %>% 
  replace_na(list(
    has_ordinary_court = FALSE,
    has_commercial_court = FALSE
  )) %>%
  group_by(oktmo_stable, year, quarter, time_id) %>% 
  summarise(
    count = sum(count),
    empl = sum(empl),
    #area = sum(area),
    #goods = sum(goods),
    #invest = sum(invest),
    #migrations = sum(migrations),
    #population = sum(population),
    #trade = sum(trade),
    #wages = sum(wages),
    has_ordinary_court = any(has_ordinary_court),
    has_commercial_court = any(has_commercial_court)
  ) %>% 
  drop_na() %>% 
  mutate(
    # Признак периода (реформа заработала в конце 2019, эффект ждем с 2020)
    post = ifelse((year >= 2019 & quarter == "Q4") | year >= 2020, 1, 0),
    # Группа (те, у кого есть суд окружного уровня)
    treat = as.numeric(has_ordinary_court),
    # Логарифмы зависимых переменных (добавляем 1, чтобы не было -Inf)
    ln_count = log(count + 1),
    #ln_rev = log(rev + 1),
    ln_empl = log(empl + 1),
    #ln_area = log(area),
    #ln_population = log(population),
    #ln_wages = log(wages),
    #ln_goods = log(goods),
    #ln_invest = log(invest),
    #ln_trade = log(trade),
  ) %>% 
  ungroup()

model_matched_q <- feols(
  ln_count ~ treat:post | oktmo_stable + year^quarter, 
  data = panel_matched_q, 
  cluster = ~oktmo_stable
)
summary(model_matched_q)

panel_matched_q$rel_q <- (as.numeric(panel_matched_q$year) - 2019) * 4 + (as.numeric(substr(panel_matched_q$quarter, 2, 2)) - 3)
model_matched_event_q <- feols(
  ln_count ~ i(rel_q, treat, ref = 0) | oktmo_stable + year^quarter,
  data = panel_matched_q,
  cluster = ~oktmo_stable
)

# 2. Визуализируем
iplot(
  model_matched_event_q, 
  main = "Event Study (Matched Sample)",
  xlab = "Год", 
  ylab = "Эффект политики"
)

ggplot(panel_matched_q, aes(x = paste0(year, quarter), y = ln_count, color = factor(treat), group = treat)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  geom_vline(xintercept = "2019Q3", linetype = "dashed", color = "red") +
  labs(title = "Тренды после мэтчинга: Число фирм", color = "Группа (1=Суд)") +
  theme_minimal()

rfsd <- read_csv("assets/rfsd_extracted_agg.csv")
count(rfsd, year, wt = count)
data %>% filter(is_sole_trader == F) %>% count(year, name = "rmsp") %>% 
  left_join(count(rfsd, year, wt = count, name = "rfsd")) %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = rmsp), col = "blue") +
  geom_line(aes(y = rfsd), col = "green")

sum(rfsd[is.na(rfsd$oktmo), "count"])
sum(rfsd$count)

rfsd_panel <- rfsd %>% 
  mutate(
    oktmo = paste0(substr(oktmo, 1, 5), "000")
  ) %>% 
  filter(count > 1) %>% 
  left_join(bdmo_oktmo, by = c("oktmo" = "oktmo_2021")) %>% 
  filter(!is.na(oktmo_stable)) %>%
  left_join(population_data) %>% 
  left_join(courts_data) %>%
  group_by(oktmo_stable, year) %>% 
  summarise(
    c = sum(count), 
    r = sum(rev), 
    has_ordinary_court = any(has_ordinary_court, na.rm = T), 
    has_commercial_court = any(has_commercial_court, na.rm = T), 
    population = median(population, na.rm = T)
  ) %>% 
  #filter(has_ordinary_court == T) %>% 
  #ggplot(aes(x = year, y = c, group = oktmo_stable)) +
  #geom_line()
  #filter(!(oktmo_stable %in% c("75701000", "36701000"))) %>% 
  group_by(oktmo_stable) %>% 
  mutate(
    ln_count = log(c + 1),
    post = ifelse(year >= 2020, 1, 0),
    treat = as.numeric(has_ordinary_court),
    median_ppl = median(population, na.rm = T),
    n_years = n(),
    size = cut(
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
  ungroup()

es <- feols(
  ln_count ~ i(year, treat, ref = 2018) | oktmo_stable + year, 
  data = rfsd_panel %>% filter(size >= "largest"), 
  cluster = ~oktmo_stable
)
summary(es)
iplot(
  es, 
  main = "Event Study: Влияние на число юрфирм (log)",
  xlab = "Год",
  sub = "Доверительные интервалы 95%. Базовый год: 2018"
)

rfsd_panel_2017_count_changes <- rfsd_panel %>% 
  filter(year <= 2017) %>% 
  select(oktmo_stable, year, c) %>% 
  pivot_wider(
    names_from = year,
    values_from = c,
    names_prefix = "count_"
  ) %>% 
  mutate(
    c1112 = count_2012 - count_2011,
    c1213 = count_2013 - count_2012,
    c1314 = count_2014 - count_2013,
    c1415 = count_2015 - count_2014,
    c1516 = count_2016 - count_2015,
    c1617 = count_2017 - count_2016,
  )

rfsd_panel_2017 <- rfsd_panel %>% 
  filter(year == 2017) %>%
  left_join(rfsd_panel_2017_count_changes) %>% 
  drop_na()

m.out <- matchit(
  treat ~ count_2011 + count_2012 + count_2013 + count_2014 + count_2015 + count_2016 + count_2017 + c1112 + c1213 + c1314 +c1415 +c1516 + c1617 + population, 
  data = rfsd_panel_2017, 
  method = "nearest", 
  distance = "glm",
  exact = c("size", "has_commercial_court"),
  ratio = 3
)
matched_data_2018 <- match.data(m.out)
matched_ids <- unique(matched_data_2018$oktmo_stable)

matched_rfsd_panel <- rfsd_panel %>% 
  filter(oktmo_stable %in% matched_ids)

model_matched <- feols(
  ln_count ~ treat:post | oktmo_stable + year, 
  data = matched_rfsd_panel,
  cluster = ~oktmo_stable
)
summary(model_matched)

es_matched <- feols(
  ln_count ~ i(year, treat, ref = 2018) | oktmo_stable + year, 
  data = matched_rfsd_panel,
  cluster = ~oktmo_stable
)
summary(es_matched)

# 2. Визуализируем
iplot(es_matched, 
      main = "Event Study (Matched Sample)",
      xlab = "Год", 
      ylab = "Эффект политики")
