## Create clean_names.csv

clean_names <- readr::read_csv(here::here("data","stock_data/stock_list.csv"),
                               col_types = readr::cols(
                                 common_name = readr::col_character(),
                                 sci_name = readr::col_character(),
                                 cc_name = readr::col_character(),
                                 stock_name = readr::col_character(),
                                 species_code = readr::col_character(),
                                 svspp = readr::col_double(),
                                 stock_season = readr::col_character(),
                                 strata = readr::col_double()
                               )) %>%
  # dplyr::filter(stock_name == !!stock_name) %>%
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
                stock_name,
                stock_season,
                stock_subarea,
                sci_name,
                cc_name,
                svspp) %>% 
  dplyr::select(-stock_season) %>% 
  dplyr::distinct(.keep_all = TRUE)

write.csv(x = clean_names, file = here::here("data-raw", "clean_names.csv"), row.names = FALSE)
