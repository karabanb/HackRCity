
load('data/full_data.Rdata')


cleaned_data <- full_data %>%
  mutate(year = as.integer(year)) %>%
  filter(year > 2010)


df_summary <- df_status(select(cleaned_data, - vars_to_drop))# variables with one unique value aren't important that's why we drop those variables

vars_to_drop <- df_summary %>% 
  filter(unique == 1) %>%
  pull(variable)


df_cor <- full_data %>%
  filter(year == 2018, !is.na(touristic_popularity)) %>%
  select(touristic_popularity, TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem)

m1 <- lm(touristic_popularity ~ TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem ,df_cor)

Metrics::rmse(df_cor$touristic_popularity, m1$fitted.values)
Metrics::mape(df_cor$touristic_popularity, m1$fitted.values)

full_data %>%
  select(TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem, year, city) %>%
  filter(!is.na(TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem)) %>%
  arrange(year) %>%
  dim()

