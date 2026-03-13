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

data <- read_delim(
  "assets/sme2026.csv", 
  delim = ";",
  col_types = cols(municipality_code = "c")
)
cities <- rbind(
  read_csv("assets/cities.csv"),
  read_csv("assets/cities_additional.csv")
)
courts <- read_csv(here("assets/courts.csv"))

area <- vroom("assets/data_Y48006001_112_v20250918.csv")
invest <- vroom("assets/data_Y48109013_112_v20250918.csv")
population <- vroom("assets/data_Y48112027_112_v20250918.csv")
wages_1 <- vroom("assets/data_Y48123007_112_v20250918.csv")
wages_2 <- vroom("assets/data_Y48423007_112_v20250918.csv")

# Prepare a mapping of data and BDMO (settlement to municipality)
bdmo_oktmo <- population %>% 
  filter(year == 2021) %>%  # approx. matches municipality code year
  distinct(municipality, oktmo, oktmo_stable) %>% 
  filter(oktmo_stable != "CD") %>% 
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
  drop_na()

# Extract necessary BDMO data portions and preprocess them
bdmo_data <- do.call(
  rbind,
  lapply(
    list(area, population, wages_1, wages_2),
    function(tbl) {
      d <- tbl %>% 
        semi_join(oktmo_mapping, by = "oktmo_stable") %>% 
        filter(year >= 2016)
      if ("mest" %in% names(d)) d <- filter(d, mest == "Городское население")
      if ("okved" %in% names(d)) d <- filter(d, okved == "Всего", indicator_period == "Январь-декабрь")
      if ("okved2" %in% names(d)) {
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
      labels = c("area", "population", "wages", "wages")
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
    population = sum(population, na.rm = TRUE),
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
  filter(settlement_type == "г", year >= 2016, year <= 2023) %>%
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

panel <- law_firms_data %>% 
  left_join(bdmo_data, by = c("oktmo_stable", "year")) %>% 
  left_join(courts_data) %>% 
  replace_na(list(
    has_ordinary_court = FALSE,
    has_commercial_court = FALSE
  )) %>%
  drop_na() %>% 
  mutate(
    # Признак периода (реформа заработала в конце 2019, эффект ждем с 2020)
    post = ifelse(year >= 2020, 1, 0),
    # Группа (те, у кого есть суд окружного уровня)
    treat = as.numeric(has_ordinary_court),
    # Логарифмы зависимых переменных (добавляем 1, чтобы не было -Inf)
    ln_count = log(count + 1),
    #ln_rev = log(rev + 1),
    ln_empl = log(empl + 1),
    ln_area = log(area),
    ln_population = log(population),
    ln_wages = log(wages)
  ) %>% 
  ungroup() %>% 
  mutate(year_num = year, year = factor(year)) %>% 
  mutate(
    size = cut(
      population,
      breaks = c(0, 5e4, 1e5, 2.5e5, 5e5, 1e6, 1e8),
      labels = c(
        "Малые (< 50 тыс.)", "Средние (50–100 тыс.)", "Большие (100–250 тыс.)",
        "Крупные (250–500 тыс. )", "Крупнейшие (500 тыс. – 1 млн)",
        "Миллионеры (> 1 млн)")
    ))

count(panel, treat)
quantile(panel$population, seq(1:10) / 10)

model_twfe <- feols(
  ln_count ~ i(size, I(treat*post), ref = "Малые (< 50 тыс.)") + ln_area + ln_population + ln_wages | oktmo_stable + year, 
  data = panel, 
  cluster = ~oktmo_stable
)
summary(model_twfe)
count(panel, size, treat)
panel[panel$treat == 1 & panel$size == "Малые (< 50 тыс.)", ]

# i() создает взаимодействия года и группы, 2018 - базовый год (контрольная точка)
es_count <- feols(
  ln_count ~ i(year, treat, ref = 2018) + treat:year_num + ln_area + ln_population + ln_wages | oktmo_stable + year, 
  data = panel, 
  cluster = ~oktmo_stable
)

iplot(es_count, 
      main = "Event Study: Влияние на число юрфирм (log)",
      xlab = "Год",
      sub = "Доверительные интервалы 95%. Базовый год: 2018")

set.seed(42)
random_cities <- sample(unique(panel$oktmo_stable), 10)
panel_placebo_group <- panel %>%
  mutate(treat_fake = ifelse(oktmo_stable %in% random_cities, 1, 0))

model_placebo_group <- feols(
  ln_count ~ treat_fake:post + ln_area + ln_population + ln_wages | oktmo_stable + year, 
  data = panel_placebo_group, 
  cluster = ~oktmo_stable
)
summary(model_placebo_group) # Ожидаем незначимый коэффициент

# Leave-one-out
# 1. Определяем список ОКТМО, которые входят в Treatment группу
treated_cities <- panel %>% 
  filter(treat == 1) %>% 
  distinct(oktmo_stable) %>% 
  pull()

# 2. Создаем контейнер для результатов
loo_results <- data.frame()

# 3. Запускаем цикл (исключаем по одному городу из 10)
for (city_to_exclude in treated_cities) {
  
  # Фильтруем данные: убираем только ОДИН город из Тритмента, 
  # но оставляем ВСЮ контрольную группу (где treat == 0)
  temp_data <- panel %>% 
    filter(oktmo_stable != city_to_exclude)
  
  # Оцениваем модель (используем вашу спецификацию)
  # Ограничение по населению (> 5e5), как вы писали, где результат значим
  res <- feols(
    ln_count ~ treat:post + ln_population + ln_wages | oktmo_stable + year, 
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
model_matched <- feols(
  ln_count ~ treat:post + ln_area + ln_population + ln_wages | oktmo_stable + year, 
  data = matched_panel, 
  cluster = ~oktmo_stable
)
summary(model_matched)

ggplot(matched_panel, aes(x = year_num, y = empl, color = factor(treat), group = treat)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "red") +
  labs(title = "Тренды после мэтчинга: Число фирм", color = "Группа (1=Суд)") +
  theme_minimal()

summarise(matched_panel, count = median(count), .by = c("treat", "year"))
ggplot(matched_panel, aes(x = count, fill = factor(treat))) +
  geom_histogram(binwidth = 100)

# Auto-matching
panel_2018 <- panel %>% filter(year == 2018, is.finite(ln_population), is.finite(ln_area)) %>% select(-size)
m.out <- matchit(treat ~ ln_population + ln_wages + ln_area, 
                 data = panel_2018, 
                 method = "nearest", 
                 distance = "glm",
                 ratio = 10) 
colSums(is.na.data.frame(panel_2018))
matched_data_2018 <- match.data(m.out)
matched_ids <- unique(matched_data_2018$oktmo_stable)
matched_panel <- panel %>% filter(oktmo_stable %in% matched_ids)
model_matched <- feols(
  ln_count ~ treat:post + ln_area + ln_population + ln_wages | oktmo_stable + year, 
  data = matched_panel, 
  cluster = ~oktmo_stable
)
summary(model_matched)
etable(model_matched)

model_es_matched <- feols(
  ln_count ~ i(year, treat, ref = 2018) + ln_population + ln_wages + ln_area | 
    oktmo_stable + year, 
  data = matched_panel, # ВАЖНО: используем только мэтчированные данные
  cluster = ~oktmo_stable
)

# 2. Визуализируем
iplot(model_es_matched, 
      main = "Event Study (Matched Sample)",
      xlab = "Год", 
      ylab = "Эффект политики")

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
