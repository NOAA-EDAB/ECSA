
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}


crop_to_strata <- function(r,stock_code, season_, mask_type = "unit"){
  
  
  s1 <- read.csv('data/stock_data/stock_list.csv', stringsAsFactors = F) %>% 
          dplyr::filter(sp == tolower(stock_code))
  
  if (nrow(s1) == 0) stop("No strata in query. Check common name spelling.")
  
  s <- s1 %>% dplyr::filter(season %in% season_)
  
  if (nrow(s) == 0) {
    stop(paste("No strata in query. Available season selections include:",unique(s1$season)))
  }
  
  # Load strata
  strata <- map_strata(stock_code = tolower(stock_code), strata = s, season = season_, save_plot = F,
                       get_sf = T) %>% 
    as("sf") %>% 
    st_transform(st_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  # Load raster and convert to stars
  r <- loadRData(file.path("data-raw",r)) 
  crs(r) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  r <- stars::st_as_stars(r)
  
  # Crop input raster to match strata
  out <- r[strata]
  
  return(out)

}
