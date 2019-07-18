fit_regime_and_trend <- function(r,
                                 stock_name,
                                 common_name,
                                 stock_season,
                                 data_season,
                                 measure_name,
                                 process_to_season = NULL,
                                 group_regex = NULL){

    if (is.null(process_to_season)){
      in_series <- 
        stars_to_series(r = r,
                        stock_name = stock_name, 
                        common_name = common_name,
                        stock_season = stock_season,
                        data_season = data_season,
                        measure_name = measure_name,
                        group_regex = group_regex) %>% 
        dplyr::filter(Time >= 1968)
      
      if (length(unique(in_series$Grouping)) > 1 ){
        return(in_series)
      } else {
        in_series <- in_series %>% 
          ecotrend::glsMs(Mean ~ Time,
                          data = .,
                          diagnostic = T,
                          fit_model  = T)
      }
      
    } else {
      in_series <-
        stars_to_series(r = r,
                        stock_name = stock_name, 
                        common_name = common_name, 
                        stock_season = stock_season,
                        data_season = data_season,
                        measure_name = measure_name,
                        process_to_season = process_to_season)
      
      if (data_season == "fall"){
        in_series <- in_series$fall %>% 
          dplyr::distinct() %>% 
          ecotrend::glsMs(Mean ~ Time,
                          data = .,
                          diagnostic = T,
                          fit_model  = T)
      } else if (data_season == "spring"){
        in_series <- in_series$spring %>% 
          dplyr::distinct() %>% 
          ecotrend::glsMs(Mean ~ Time,
                          data = .,
                          diagnostic = T,
                          fit_model  = T)
      }
    }
 
  
  STARS_in <- in_series$fit %>% 
    dplyr::select(-fit) %>% 
    as.matrix()
  
  if (in_series$`selection summary`$pval[1] < 0.05){
    mod <- in_series$model
    
    in_series <- in_series$fit %>% 
      dplyr::select(Time = time, 
                    Series = series,
                    Trend = fit)
    
    summ_ <- gls_summary(mod =  mod)
    trend <- summ_$Value[1]
    lci <- summ_$Value[2]
    uci <- summ_$Value[3]
    
    plot_title <- sprintf("%s Trend: %s CI: (%s, %s)", 
                          stringr::str_to_title(data_season),
                          trend,
                          lci,
                          uci)
    
  } else {
    in_series <- in_series$fit %>% 
      dplyr::select(Time = time, 
                    Series = series)
    plot_title <- stringr::str_to_title(data_season)
  }
  
  out <- 
    in_series %>%  
    left_join(.,STARS(mat.in = STARS_in),by = "Time") %>% 
    dplyr::select(-Data, -RSI, -Regime.Number,
                  `Regime mean` = Regime.Mean) %>% 
    dplyr::select(which(sapply(.,var)!=0))
  
  return(list(df = out,
              plot_title = plot_title))
}
