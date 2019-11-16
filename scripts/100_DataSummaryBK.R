
load('data/full_data.Rdata')
load('data/GUS_additional_data.RData')
load('data/hotels.Rdata')
load('data/restaurants.Rdata')
load('data/things_to_do.Rdata')

cleaned_data <- full_data %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem)) 

gus_data <- GUS_additional_data %>%
  group_by(city) %>%
  summarise_all(list(mean), na.rm = T)


df_summary <- df_status(cleaned_data)# variables with one unique value aren't important that's why we drop those variables
df_summary_gus <- df_status(gus_data)


vars_to_drop <- df_summary %>% 
  filter(unique ==  1) %>%
  pull(variable)

wiki <- full_data %>%
  select(city, starts_with('wiki')) %>%
  distinct() %>%
  filter(!is.na(wikipedia_area))

cleaned_data <- cleaned_data %>% 
  select(-vars_to_drop, -starts_with('wiki')) %>%
  left_join(.,wiki, by = 'city') %>%
  left_join(., gus_data, by = 'city') %>%
  left_join(., hotels, by = 'city') %>%
  left_join(., restaurants, by =  'city') %>%
  left_join(., things_to_do, by = 'city')

save(cleaned_data, file = 'data/100_CleanedData.Rdata')

