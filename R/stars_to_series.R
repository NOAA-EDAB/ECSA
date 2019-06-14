stars_to_series <- function(r, 
                            stock_name,
                            common_name,
                            stock_area, 
                            measure_name, 
                            data_season,
                            process_to_season = NULL,
                            group_regex = NULL){
  
  if (stock_area != "both"){
    if (stringr::str_detect(r, "fall") & data_season == "fall" & stock_area %in% "fall"){
      stock_area <- "fall"
    } else if (stringr::str_detect(r, "spring") & data_season == "spring" & stock_area %in% "spring"){
      stock_area <- "spring"
    }
  }
  
  message(paste("Data choice:", r, "\nStock area:",stock_area))
    
  cube_in <- try(
        crop_to_strata(r = r,
                       stock_name = stock_name,
                       stock_area = stock_area) %>%
          as.tbl_cube()
        )
      if (class(cube_in) == "try-error"){
        message("Double check stock_area")
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
