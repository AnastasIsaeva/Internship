# Скачаем данные
original_data_all <-read_excel("АА.xlsx", sheet = 1) %>% 
  left_join(read_excel("АА.xlsx", sheet = 2))


# преобразуем данные
mutated_data_all <- original_data_all %>% 
  transmute(
    ID,
    Sex = recode_factor(Пол,
                        "Мужской" = "Male",
                        "Женский" = "Female"),
    Age = `Возраст на момент диагноза, лет`,
    Degree = recode_factor(`Степень тяжести`,
                           "Сверхтяжёлая" = "Very severe",
                           "Тяжёлая" = "Severe"),
    ATG_Start_Date = `Дата начала АТГ`,
    Therapy = recode_factor(`Ветвь рандомизации`,
                            "Стандартная ИСТ" = "IST",
                            "Элтромбопаг" = "ELTR + IST"),
    OR = as.factor(`Частичный ответ`),
    OR_Date = `Дата частичного ответа`,
    four_months_OR = case_when(
      `Частичный ответ` == 1 &
        as.numeric(`Дата частичного ответа` - `Дата начала АТГ`) < 128 ~ 1, 
      TRUE ~ 0
    ),
    CR = as.factor(`Полный ответ`),
    CR_Date = `Дата полного ответа`,
    four_months_CR = case_when(
      `Полный ответ` == 1 &
        as.numeric(`Дата полного ответа` - `Дата начала АТГ`) < 128 ~ 1, 
      TRUE ~ 0
    ),
    Visit = recode_factor(Визит,
                          "Инициально (скрининг)" = "Screening",
                          "1 день 1 курса АТГ" = "1 day",
                          "1 нед" = "1 week",
                          "2 нед" = "2 week"),
    Date = case_when(
      Дата == "Нет данных" ~ NA,
      TRUE ~ as.POSIXct(as.numeric(Дата) * 86400, origin = "1899-12-30", tz = "UTC")
    ),
    WBC = as.numeric(`Лейкоциты (WBC), 10^9/л`),
    NEU = as.numeric(`Нейтрофилы (NEU), 10^9/л`)
  ) %>% 
  mutate(
    four_months_OR = as.factor(four_months_OR),
    four_months_CR = as.factor(four_months_CR),
    Percent_NEU_to_WBC = round(NEU/WBC*100, 2))

# Выберем данные без лейкоцитов
mutated_data <- mutated_data_all %>%
  select(ID, Sex, Age, Degree, ATG_Start_Date, OR, OR_Date, CR, CR_Date, four_months_OR, four_months_CR, Therapy) %>%
  distinct(ID, .keep_all = TRUE)

