library(dplyr)
library(readr)
library(tidyr)
library(magrittr)

## stock strata list from Jessica Blaylock (October 4, 2018)
sdat <- readr::read_csv(here::here("data-raw/stocks-strata_10042018.csv"),
                 col_types = cols(
                   group = col_character(),
                   SVSPP = col_double(),
                   Species = col_character(),
                   Stock = col_character(),
                   `Season(s)` = col_character(),
                   `Strata set` = col_character()
                 ))

sdat_clean <- sdat %>%
  dplyr::select(svspp = SVSPP, species = Species, stock = Stock, season = `Season(s)`, strata_set = `Strata set`) %>%
  na.omit() %>%
  separate_rows(strata_set, sep = ";") %>%
  mutate(season = tolower(season),
         strat = case_when(grepl("offshore", tolower(strata_set)) ~ "1",
                           grepl("inshore", tolower(strata_set))~ "3",
                           TRUE ~ NA_character_),
         strata_set = gsub("[[:alpha:]]", "", strata_set)) %>%
  separate_rows(strata_set, sep = ",") %>%
  separate(strata_set, sep = "-", c("low", "hi"), remove = FALSE) %>%
  mutate(low = gsub("/s", "", low),
         hi = gsub("/s", "", hi),
         hi = ifelse(is.na(hi),
                     low,
                     hi),
         index = 1:n(),
         range = purrr::map(index, function(x) low[x]:hi[x])) %>%
  unnest(range) %>%
  mutate(strata = sprintf("%s%02d0", strat, range))


sdat_names <- sdat_clean %>%
  separate(species, sep = "\\s+(?=[[:upper:]])", c("common_name", "sci_name")) %>%
  mutate(common_name = tolower(common_name)) %>%
  tidyr::extract(common_name, into = c('partA', 'partB'), '(.*)\\s+([^ ]+)$', remove = FALSE) %>%
  dplyr::mutate(partA = substr(partA, start = 1, stop = 3),
                partB = substr(partB, start = 1, stop = 3),
                partC = ifelse(is.na(partA) | is.na(partB),
                               substr(common_name, start = 1, stop = 6),
                               paste0(partA, partB)),
                species_code = stringr::str_pad(partC, 6, pad = "z", side = "right"),
                species_code = species_code,
                cc_name = gsub(" ", "-", common_name),
                stock = tolower(stock),
                stock = gsub("/", "-", stock),
                stock_name = case_when(stock == "unit" ~ cc_name,
                                       stock != "unit" ~ sprintf("%s_%s", 
                                                                 cc_name, 
                                                                 stock))) %>%
  dplyr::select(common_name, sci_name, cc_name, stock_name, species_code, svspp, stock) %>%
  distinct(.keep_all = TRUE)


stock_list <- sdat_clean %>%
  mutate(season = case_when(grepl("spring,\\sfall", season) ~ "both",
                            season == "spring" ~ "spring",
                            season == "fall" ~ "fall",
                            TRUE ~ NA_character_),
         stock = tolower(stock),
         stock = gsub("/", "-", stock)) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(sdat_names, by = c("svspp", "stock")) %>%
  dplyr::select(common_name, sci_name, cc_name, stock_name, species_code, svspp, season, strata) %>%
  dplyr::rename(stock_season = season)

#Adding menhaden strata
menhaden <- read.csv(here::here("data-raw/menhaden_strata_raw.csv"), stringsAsFactors = F) %>% 
  dplyr::filter(sp == "atlmen") %>% 
  dplyr::mutate(common_name = "atlantic menhaden",
                sci_name = "Brevoortia tyrannus",
                cc_name = "atlantic-menhaden",
                strata = as.character(strata)) %>% 
  # dplyr::mutate(stock_name = paste(cc_name, stock_area, sep = "_")) %>% 
  tidyr::unite(.,"stock_name", cc_name,stock_area, sep = "_") %>% 
  dplyr::mutate(stock_name = stringr::str_remove(stock_name, "_unit"),
                svspp = 36,
                cc_name = "atlantic-menhaden") %>% 
  dplyr::rename(species_code = sp,
                stock_season = season)

stock_list %<>% bind_rows(menhaden)


stock_list <- stock_list %>%
  dplyr::mutate(stock_subarea = dplyr::case_when(grepl("_sne-ma$", stock_name) ~ "southern New England/mid-Atlantic ",
                                                 grepl("_cc$", stock_name) ~ "Cape Cod ",
                                                 grepl("_sne$", stock_name) ~ "southern New England ",
                                                 grepl("_gom-ngb$", stock_name) ~ "Gulf of Maine/northern Georges Bank ",
                                                 grepl("_sgb-ma$", stock_name) ~ "southern Georges Bank/mid-Atlantic ",
                                                 grepl("_gb$", stock_name) ~ "Georges Bank ",
                                                 grepl("_gom-ngb$", stock_name) ~ "Gulf of Maine/northern Georges Bank ",
                                                 grepl("_gom$", stock_name) ~ "Gulf of Maine ",
                                                 grepl("_gom-gb$", stock_name) ~ "Gulf of Maine/Georges Bank ",
                                                 grepl("_north$", stock_name) ~ "north ",
                                                 grepl("_south$", stock_name) ~ "south ",
                                                 TRUE ~ ""),
                common_name = gsub("^atlantic", "Atlantic", common_name),
                common_name = gsub("^american", "American", common_name),
                common_name = gsub("^acadian", "Acadian", common_name)) %>% 
  dplyr::select(common_name,
                sci_name,
                cc_name,
                stock_name,
                species_code,
                svspp,
                stock_season,
                strata,
                stock_subarea)# %>% 
# dplyr::filter(stock_name == !!stock_name) %>%
  # dplyr::select(-stock_season) %>%
  # dplyr::distinct(.keep_all = TRUE)

write.csv(here::here("data/stock_data/stock_list.csv"), x = stock_list, row.names = FALSE)

