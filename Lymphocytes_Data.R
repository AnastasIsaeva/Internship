# Скачаем данные
original_data_all <-read_excel("АА.xlsx", sheet = 1) %>% 
  left_join(read_excel("АА.xlsx", sheet = 2), join_by(ID))


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
    Date = as.POSIXct(as.numeric(na_if(Дата, "Нет данных")) * 86400, origin = "1899-12-30", tz = "UTC"),
    WBC = as.numeric(na_if(`Лейкоциты (WBC), 10^9/л`, "Нет данных")),
    NEU = as.numeric(na_if(`Нейтрофилы (NEU), 10^9/л`, "Не делали"))
  ) %>% 
  mutate(
    four_months_OR = as.factor(four_months_OR),
    four_months_CR = as.factor(four_months_CR),
    Percent_NEU_to_WBC = round(NEU/WBC*100, 2))

# Выберем данные без лейкоцитов
mutated_data <- mutated_data_all %>%
  select(ID, Sex, Age, Degree, ATG_Start_Date, OR, OR_Date, CR, CR_Date, four_months_OR, four_months_CR, Therapy) %>%
  distinct(ID, .keep_all = TRUE)


# Исключаем выбросы(ошибки), заменяем 0 в WBC и NEU

filtered_data_all <- mutated_data_all %>%
  group_by(ID) %>%
  filter(
    # Для визита "1 day" оставляем строки, где разница ≤ 5 дней
    !(Visit == "1 day" & 
        !is.na(ATG_Start_Date - Date) & 
        abs(as.numeric(difftime(Date, ATG_Start_Date, units = "days"))) > 5),
    
    # Для визита "1 week" проверяем разницу с "1 day"
    !(Visit == "1 week" & 
        !is.na(Date - Date[match("1 day", Visit)]) & 
        as.numeric(difftime(Date, Date[match("1 day", Visit)], units = "days")) >= 15),
    
    # Для визита "2 week" проверяем разницу с "1 day"
    !(Visit == "2 week" & 
        !is.na(Date - Date[match("1 day", Visit)]) & 
        as.numeric(difftime(Date, Date[match("1 day", Visit)], units = "days")) >= 23),
    
    # Исключаем строки с NA в WBC или NEU
    !is.na(WBC),
    !is.na(NEU)
  ) %>%
  ungroup() %>%
  # Заменяем 0 в WBC и NEU на половину минимального положительного значения
  mutate(
    WBC = ifelse(WBC == 0, min(WBC[WBC > 0], na.rm = TRUE)/2, WBC),
    NEU = ifelse(NEU == 0, min(NEU[NEU > 0], na.rm = TRUE)/2, NEU)
  )

filtered_data_all %>%
  assert(not_na, ID, Visit) %>%
  verify(is_uniq(ID, Visit)) %>%
  assert(not_na, WBC, NEU) %>%
  assert(within_bounds(0, Inf, include.lower = FALSE), WBC, NEU)
