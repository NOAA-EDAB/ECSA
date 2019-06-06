stars_to_series <- function(r, stock_code, measure_name){
  
  #Generic tbl_cube to data.frame conversion
  season_in <- stringr::str_extract(r, "fall|spring")

  cube_in <- try(
      crop_to_strata(r = r,
                     stock_code = stock_code, season_ = season_in) %>% 
        as.tbl_cube() 
    )
    if (class(cube_in) == "try-error"){
      
      season_default <- ifelse(season_in =="fall",
             "spring",
             ifelse(season_in == "spring",
                    "fall",
                    "both"))
      
      cube_in <- try(
        crop_to_strata(r = r,
                       stock_code = stock_code, season_ = season_default) %>% 
          as.tbl_cube()
      )
      
      message(paste("\n",stringr::str_to_title(season_in),"data clipped to",
                    season_default,"stock areas"))
      
    }

  names(cube_in$mets) <- measure_name
  
  df_out <- cube_in %>% 
    group_by(band) %>% 
    dplyr::summarise(Mean = mean(get(measure_name), na.rm = T)) %>% 
    as.data.frame() %>% 
    mutate(Time = as.numeric(str_extract(band, "\\d{4}"))) %>% 
    dplyr::select(-band)
  
  return(df_out)

}
