library(Metrics)

load('data/full_data.Rdata')


cleaned_data <- full_data %>%
  mutate(year = as.integer(year)) %>%
  filter(year > 2010)


df_summary <- df_status((cleaned_data))# variables with one unique value aren't important that's why we drop those variables

vars_to_drop <- df_summary %>% 
  filter(unique == 1) %>%
  pull(variable)


df_cor <- full_data %>%
  filter(year == 2018, !is.na(touristic_popularity)) %>%
  select(touristic_popularity, TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem)

df_2500 <- df_cor %>% filter(touristic_popularity < 2500)
df_1500 <- df_cor %>% filter(touristic_popularity < 1500)
cor(df_cor)[1,]
cor(df_2500)[1,]
cor(df_1500)[1,]
plot(df_2500)
plot(df_1500)

library(mgcv)
m1 <- gam(touristic_popularity ~ s(TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem), data = df_2500)
m2 <- gam(touristic_popularity ~ s(TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem), data = df_1500)

Metrics::rmse(df_2500$touristic_popularity, m1$fitted.values)
Metrics::mape(df_2500$touristic_popularity, m1$fitted.values)

Metrics::rmse(df_1500$touristic_popularity, m2$fitted.values)
Metrics::mape(df_1500$touristic_popularity, m2$fitted.values)


full_data %>%
  select(TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem, year, city) %>%
  filter(!is.na(TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem)) %>%
  arrange(year) %>%
  dim()

