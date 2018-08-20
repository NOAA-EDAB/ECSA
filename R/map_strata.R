
map_strata <- function(common_name, spring_strata, fall_strata, overwrite = FALSE, save_plot) {
  
  library(ggplot2)
  library(viridis)
  library(ggthemes)
  
  ## General mapping parameters
  xmin = -77
  xmax = -65
  ymin = 35
  ymax = 45
  
  xlims <- c(xmin, xmax)
  ylims <- c(ymin, ymax)
  crs <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  ## Download data layers
  
  ## 1) Strata  
  source("R/get_strata.R")
  get_strata(overwrite = overwrite)
  
  ## 2) North America layer
  ne_countries <- rnaturalearth::ne_countries(scale = 10,
                                              continent = "North America",
                                              returnclass = "sf") %>% 
    sf::st_transform(crs = crs)
  
  ## 3) State layer
  ne_states <- rnaturalearth::ne_states(country = "united states of america",
                                        returnclass = "sf") %>% 
    sf::st_transform(crs = crs)
  
  ## 
  
  strata_grid <- sf::st_read("data/BTS_Strata.shp",
                             quiet = TRUE) %>% 
    mutate(SEASON = case_when(STRATA %in% intersect(strata_spring, strata_fall) ~ "spring and fall",
                              STRATA %in% strata_spring ~ "spring",
                              STRATA %in% strata_fall ~ "fall",
                              TRUE ~ NA_character_)) %>% 
    filter(!is.na(SEASON)) %>% 
    mutate(SEASON = tolower(SEASON),
           SEASON = factor(SEASON, levels = c("spring", "fall", "spring and fall")))
  
  
  p1 <- ggplot() +
    geom_sf(data = strata_grid, aes(fill = SEASON), size = 0.05, color = "grey40") +
    geom_sf(data = ne_countries, color = "grey60", size = 0.25) +
    geom_sf(data = ne_states, color = "grey60", size = 0.05) +
    scale_fill_viridis(discrete = TRUE) +
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    theme_map() +
    labs(title = sprintf("%s strata", common_name),
         fill = "Season") +
    theme(legend.position = "bottom") +
    theme(legend.key.width = unit(2, "cm"))
  
  if(save_plot) {
    ggsave(p1, sprintf("%s_strata-map.png", common_name), type = "cairo")
  }
  return(p1)
} 
