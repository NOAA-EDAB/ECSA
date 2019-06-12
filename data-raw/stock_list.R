library(dplyr)
library(readr)
library(tidyr)

## stock strata list from Jessica Blaylock
sdat <- readr::read_csv("data-raw/stocks-strata_10042018.csv",
                 col_types = cols(
                   group = col_character(),
                   SVSPP = col_double(),
                   Species = col_character(),
                   Stock = col_character(),
                   `Season(s)` = col_character(),
                   `Strata set` = col_character()
                 ))

sdat_clean <- sdat %>%
  select(svspp = SVSPP, species = Species, stock = Stock, season = `Season(s)`, strata_set = `Strata set`) %>%
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
  mutate(strat = sprintf("%s%02d0", strat, range))


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
  select(common_name, sci_name, cc_name, stock_name, species_code, svspp) %>%
  distinct(.keep_all = TRUE)


stock_list <- sdat_clean %>%
  mutate(season = case_when(grepl("spring,\\sfall", season) ~ "both",
                            season == "spring" ~ "spring",
                            season == "fall" ~ "fall",
                            TRUE ~ NA_character_),
         stock = tolower(stock)) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(sdat_names) %>%
  select(common_name, sci_name, cc_name, stock_name, species_code, svspp, season, strat, stock)

write.csv("data/stock_data/stock_list.csv", x = stock_list, row.names = FALSE)

