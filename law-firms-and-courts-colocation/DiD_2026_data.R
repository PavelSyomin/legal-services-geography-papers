library(dplyr)
library(fixest)
library(ggplot2)
library(here)
library(MatchIt)
library(nanoparquet)
library(readr)
library(stringr)
library(tidyr)
library(vroom)
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


# Misc controls
area <- vroom("assets/data_Y48006001_112_v20250918.csv")
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
  group_by(oktmo_stable, year) %>% 
  summarise(across(count:empl, sum))

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
      )
    )
  ) %>% 
  ungroup() %>% 
  filter(
    n_years >= 9
  )

model <- feols(
  ln_count ~ treat:post | oktmo_stable + year, 
  data = panel,
  cluster = ~oktmo_stable
)
summary(model)

es <- feols(
  ln_count ~ i(year, treat, ref = 2018) | oktmo_stable + year, 
  data = panel, 
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
panel_2018_count_changes <- panel %>% 
  filter(year <= 2018) %>% 
  select(oktmo_stable, year, count) %>% 
  pivot_wider(
    names_from = year,
    values_from = count,
    names_prefix = "count_"
  ) %>% 
  mutate(
    change_2016_2017 = count_2017 - count_2016,
    change_2017_2018 = count_2018 - count_2017,
  )
panel_2018 <- panel %>% 
  filter(year == 2018) %>%
  left_join(panel_2018_count_changes) %>% 
  drop_na()

m.out <- matchit(
  treat ~ count_2016 + count_2017 + count_2018 + change_2016_2017 + change_2017_2018 + ln_population, 
  data = panel_2018, 
  method = "nearest", 
  distance = "glm",
  exact = c("has_commercial_court", "size"),
  ratio = 3
)
matched_data_2018 <- match.data(m.out)
matched_ids <- unique(matched_data_2018$oktmo_stable)
matched_panel <- panel %>% filter(oktmo_stable %in% matched_ids)
matched_panel %>% 
  group_by(oktmo_stable, treat) %>% 
  arrange(year, .by_group = T) %>% 
  summarise(c = last(ln_population) - first(ln_population)) %>% 
  arrange(-c)

matched_panel %>% 
  filter(!(oktmo_stable %in% c("07727000", "03701000", "71701000", "03726000", "58701000", "04701000"))) %>% 
  group_by(year, treat) %>% 
  summarise(ppl = median(ln_population)) %>% 
  ggplot(aes(x = year, y = ppl, col = factor(treat), group = treat)) +
  geom_line()

model_matched <- feols(
  ln_count ~ treat:post | oktmo_stable + year, 
  data = matched_panel, #%>% filter(!(oktmo_stable %in% c("07727000", "03701000", "71701000", "03726000", "58701000", "04701000"))), 
  cluster = ~oktmo_stable
)
summary(model_matched)

es_matched <- feols(
  ln_count ~ i(year, treat, ref = 2018) | oktmo_stable + year, 
  data = matched_panel, #%>% filter(!(oktmo_stable %in% c("07727000", "03701000", "71701000", "03726000", "58701000", "04701000"))),
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


# Matching
manual_pairs <- data.frame(
  treat = sort(unique(panel[panel$treat == 1, ]$oktmo_stable)),
  control = c(
    "04701000", "98701000", "08701000", "46790000",
    "57701000", "53701000", "52701000", "80701000", 
    "71701000", "60701000"
  )
)

matched_panel <- panel %>% filter(oktmo_stable %in% unlist(manual_pairs))
matched_panel$treated <- matched_panel$treat * matched_panel$post
model_matched <- feols(
  ln_count ~ treated + ln_area + ln_population + ln_wages | treat + post, 
  data = matched_panel, 
  #cluster = ~oktmo_stable
)
summary(model_matched)

ggplot(matched_panel, aes(x = year, y = ln_count, color = factor(treat), group = treat)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "red") +
  labs(title = "Тренды после мэтчинга: Число фирм", color = "Группа (1=Суд)") +
  theme_minimal()

summarise(matched_panel, count = median(count), .by = c("treat", "year"))
ggplot(matched_panel, aes(x = count, fill = factor(treat))) +
  geom_histogram(binwidth = 100)



plot(summary(m.out), 
     var.names = TRUE, 
     abs = TRUE, 
     main = "Качество мэтчинга (SMD)")
# Если нужно добавить линию порога, используйте abline() после вызова plot
abline(v = 0.1, lty = 2, col = "red")

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
