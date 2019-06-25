
plot_sae <- function(data_season, biomass_, svspp_, include_zeros = F, k, fill = NA,
                     exclude_treatments = NA, include_legend = T, legend_title){
  
  `%>%` <- magrittr::`%>%`
  #swept area estimates
  spring_sae <- read.csv(here::here("data-raw/spring_sae_tb_sum.csv"), stringsAsFactors = F)
  fall_sae <- read.csv(here::here("data-raw/fall_sae_tb_sum.csv"), stringsAsFactors = F)
  
  if (data_season == "fall"){
    df <- fall_sae
    rm(fall_sae);rm(spring_sae)
  } else if (data_season == "spring"){
    df <- spring_sae
    rm(fall_sae);rm(spring_sae)
  }
  
  Var_ <- ifelse(!include_zeros,"ztotal","total")
  ylab <- ifelse(biomass_ == "yes", "Biomass, log10 kg", "Abundance, log10 N")
  leg.title <- ifelse(include_legend, legend_title, "")
  
  
  sae_all <- 
    df %>%
    dplyr::filter(svspp == svspp_,
                  biomass == biomass_,
                  !treatment %in% exclude_treatments) %>% 
    dplyr::select(-svspp, -biomass) %>%
    tidyr::unite(., "stock_treatment",stocks, treatment,  sep = "_") %>%
    tidyr::gather(.,Var, Value, -year,-stock_treatment) %>% 
    tidyr::separate(., stock_treatment, into = c("area","treatment"), sep = "_") %>% 
    dplyr::rename(Time = year) %>% 
    dplyr::filter(Var == Var_) %>% 
    dplyr::mutate(treatment = as.factor(treatment),
                  Value_log10 = log10(Value))
  
  
  sae_smoothed <- 
    sae_all %>% 
    dplyr::group_by(area, Time) %>% 
    dplyr::mutate(Value_log10 = ifelse(is.infinite(Value_log10), NA, Value_log10)) %>% 
    dplyr::summarise(group_mean = mean(Value_log10, na.rm = T)) %>% 
    dplyr::mutate(smoothed_group_mean_log10 = zoo::rollmean(group_mean, k = k, fill = fill))
  
  sae_all$Value_log10 <- round(sae_all$Value_log10, 3)
  sae_smoothed$smoothed_group_mean_log10 <- round(sae_smoothed$smoothed_group_mean_log10,
                                                  3)
  sae_all$area <- as.factor(sae_all$area)
  
  out <- 
    
    ggplot2::ggplot() +
    ggplot2::geom_line(data = sae_smoothed,
                       ggplot2::aes(x = Time,
                                    y = smoothed_group_mean_log10,
                                    color = area)) +
    ggplot2::geom_point(data = sae_all,
                        ggplot2::aes(x = Time,
                                     y = Value_log10,
                                     color = area),
                        alpha = 0.25) +
    
    ggplot2::guides(color = guide_legend(title = leg.title,
                                         override.aes = list(shape = NA,
                                                             linetype = "solid"))) +
    ggplot2::ylab(ylab) +
    theme_bw()
  
  (p <- plotly::ggplotly(out))
  
  if (!include_legend){
    
    
    for (i in 1:length(p$x$data)){
      p$x$data[[i]]$text <- c(p$x$data[[i]]$text, "") 
      p$x$data[[i]]$showlegend <- FALSE
    }
  }
  
  
  return(p)
}
