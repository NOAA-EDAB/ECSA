stars_to_series <- function(r, stock_name,
                            common_name,
                            stock_area, measure_name, process_to_season = NULL, group_regex = NULL){
  
  
  #This takes the season of the data set out of the file name and passes it to crop_to_strata(). It
  
  if (is.null(process_to_season)){
      #Generic tbl_cube to data.frame conversion
      season_in <- stringr::str_extract(r, "fall|spring")
  } else {
      season_in <- process_to_season
  }
  
  
    cube_in <- try(
        crop_to_strata(r = r,
                       stock_name, stock_area, common_name, season_ = season_in) %>% 
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
                         stock_name, stock_area, common_name, season_ = season_default) %>% 
                         as.tbl_cube()
        )
        
        message(paste("\n",stringr::str_to_title(season_in),"data clipped to",
                      season_default,"stock areas"))
        
      }

  
      # If fall and spring are not previously defined, we need to aggregate them here. 
      # Process_to_season can be "fall","spring", or "both".
      
      if (!is.null(process_to_season)){
        
        # Process spring data (March, April, May)
        cube_in_spring <- cube_in %>% filter(stringr::str_detect(band, "(03\\.01)|(04\\.01)|(05\\.01)"))
        names(cube_in_spring$mets) <- measure_name
        
        df_out_spring <- cube_in_spring %>% 
          group_by(band) %>%
          dplyr::summarise(Monthly_mean = mean(get(measure_name), na.rm = T)) %>%
          as.data.frame() %>% 
          mutate(Time = as.numeric(str_extract(band, "\\d{4}"))) %>% 
          dplyr::select(-band) %>% 
          dplyr::group_by(Time) %>% 
          dplyr::summarise(Mean = mean(Monthly_mean, na.rm = T))
        
        # Process fall data (September, October, November)
        cube_in_fall <- cube_in %>% filter(stringr::str_detect(band, "(09\\.01)|(10\\.01)|(11\\.01)"))
        names(cube_in_fall$mets) <- measure_name
        
        df_out_fall <- cube_in_fall %>% 
          group_by(band) %>%
          dplyr::summarise(Monthly_mean = mean(get(measure_name), na.rm = T)) %>%
          as.data.frame() %>% 
          mutate(Time = as.numeric(str_extract(band, "\\d{4}"))) %>% 
          dplyr::select(-band) %>% 
          dplyr::group_by(Time) %>% 
          dplyr::summarise(Mean = mean(Monthly_mean, na.rm = T))
        
        return(list("spring" = df_out_spring,
                    "fall" = df_out_fall))
      } else {

        names(cube_in$mets) <- measure_name        
        
        df_out <- cube_in %>% 
          group_by(band) %>% 
          dplyr::summarise(Mean = mean(get(measure_name), na.rm = T)) %>%
          as.data.frame() %>% 
          mutate(Time = as.numeric(str_extract(band, "\\d{4}")))
        
          if (!is.null(group_regex)) {
            df_out <- df_out %>% 
              mutate(Grouping = stringr::str_extract(band, group_regex)) %>% 
              dplyr::select(-band)
          } else {
            df_out <- dplyr::select(df_out, -band)
          }
        
        return(df_out)
  }
}
