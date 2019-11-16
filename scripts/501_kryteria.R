library(dplyr)
library(stringr)

full_data_b <- full_data
full_data_b <- full_data_b %>% filter(year == 2018)
# usuwamy gdzie tylko zera
full_data_b <- full_data_b[, colSums(full_data_b != 0, na.rm = T) > 0]

full_data_b$KINA <- full_data_b$KINA.wlasnosc_miasta_ogolem + full_data_b$KINA.wlasnosc_wojewodztwa_ogolem
full_data_b <- full_data_b %>% select(-str_which(names(full_data_b), 'KINA.'))

full_data_b <- full_data_b %>% 
  select(city,
         TRANSPORT_MORSKI.Ruch_pasazerow_w_portach_morskich_krajowy_przyjazdy,
        DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_020_Lesnictwo,
         DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_550_Hotele_i_restauracje,
         DOCHODY_BUDZETOW_MIAST.Dochody_wg_kat_budzetu_Dzial_925_Ogrody_botan_i_zoolog_oraz_naturalne_obszary_i_obiekty_chronionej_przyrody,
        TURYSTYCZNE_OBIEKTY.hostele_obiekty_ogolem,
        `TURYSTYCZNE_OBIEKTY.obiekty_ogolem_hotele_kategorii_*****`,
        `TURYSTYCZNE_OBIEKTY.obiekty_ogolem_hotele_kategorii_****`,
        TURYSTYCZNE_OBIEKTY.Placowki_gastronomiczne_restauracje,
        wikipedia_nr_of_people,
        wikipedia_height,
        budzet_wydatki_gospodarka_komunalna_ochrona_srodowiska_utrzymanie_zieleni_w_miastach_i_gminach,
        budzet_wydatki_kultura_chrona_dziedzictwa_narodowego_ochrona_zabytkow_i_opieka_nad_zabytkami,
        budzet_wydatki_kultura_chrona_dziedzictwa_narodowego_galerie_i_biura_wystaw_artystycznych,
        budzet_wydatki_kultura_chrona_dziedzictwa_narodowego_teatry,
        budzet_wydatki_kultura_chrona_dziedzictwa_narodowego_muzea,
        Imprezy_masowe.liczba_imprez_ogolem,
        Imprezy_masowe.Liczba_uczestnikow_imprez_ogolem,
        KINA, 
        MUZEA.Muzea_w_gestii_samorzadu_oddzialy,
        `SCIEZKI_ROWEROWE.sciezki_rowerowe_(drogi_dla_rowerow)_ogolem`)

full_data_b$TRANSPORT_MORSKI.Ruch_pasazerow_w_portach_morskich_krajowy_przyjazdy <- 
  full_data_b$TRANSPORT_MORSKI.Ruch_pasazerow_w_portach_morskich_krajowy_przyjazdy > 0
colnames(full_data_b) <- c('city','Morze', 'Dochody_z_lesnictwa', 'Dochody_z_hoteli_i_restauracji',
                           'Dochody_z_zoo_i_botan', 'Hostele', 'Hotele5g', 'Hotele4g',
                           'Restauracje', 'Ludnosc', 'Wysokosc', 'Wydatki_na_zielen',
                           'Wydatki_na_zabytki', 'Wydatki_na_sztuke', 'Wydatki_na_teatr',
                           'Wydatki_na_muzea', 'Liczba_imprez', 'Liczba_uczest_imprez',
                           'Kina', 'Muzea', 'Sciezki_rowerowe')
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

full_data_c <- as.data.frame(apply(X = full_data_b[,2:ncol(full_data_b)], FUN = normalize,MARGIN = 2))
full_data_c <- full_data_c %>%
  mutate(SuperHotele = (Hotele5g+Hotele4g)/2,
         Natura = (Dochody_z_lesnictwa+Dochody_z_zoo_i_botan+Wydatki_na_zielen)/3,
         Historia = (Wydatki_na_muzea+Muzea)/2,
         Tlum = (Ludnosc+Liczba_uczest_imprez+Liczba_imprez)/3,
         Restauracje = (Restauracje+Dochody_z_hoteli_i_restauracji)/2,
         Teatr = (Kina+Wydatki_na_teatr)/2) %>%
  select(Morze, Natura, SuperHotele, Hostele, Historia, Tlum, Restauracje,
         Ludnosc, Wysokosc, Wydatki_na_zabytki, Wydatki_na_sztuke,
         Teatr, Sciezki_rowerowe, Liczba_imprez)
full_data_c <- cbind(full_data_b$city, full_data_c)
colnames(full_data_c) <- c('city', 'Morze', 'Natura', 'SuperHotele', 'Hostele',
                           'Historia','Tlum','Restauracje','Wielkosc_miasta', 'Wysokosc',
                           'Pomniki','Sztuka','Teatr','Rowery','Imprezy')
statystyki <- full_data_c

statystyki$Miasto_rodzinne <- as.numeric(statystyki$city %in% c('poznan', 'kumquat', 'goji_berry'))
write.csv(statystyki, file = 'data/statystyki.csv',row.names = F)
