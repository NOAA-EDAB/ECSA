#'A function to mask ecosystem RasterStacks with stock-specific strata of interest. Outputs \code{RasterStacks} as \code{stars} objects for further aggregation.
#'
#'@param r Filename of an .rdata file containing a RasterStack of ecosystem data to be masked.
#'@param stock_name Name of stock to be queried from 'data/stock_data/stock_list.csv'. 
#'@param stock_season Seasonal designation for strata. Can be \code{"fall"}, \code{"spring"}, or \code{"both"}. However, all seasons do not apply to all stocks.
#'@param common_name Common name of species of interest.
#'
#'@return 
#'
#'@importFrom magrittr "%>%"
#'

crop_to_strata <- function(r, stock_name, stock_season, common_name){
  
  s1 <- read.csv(here::here('data','stock_data','stock_list.csv'),
                 stringsAsFactors = F) %>% 
    dplyr::filter(stock_name == !!stock_name)
  
  if (nrow(s1) == 0) stop("No strata in query. Check common name spelling.")
  
  strata <- s1 %>% 
    dplyr::filter(stock_season %in% stock_season) %>% 
    dplyr::select(strata, stock_season)
  
  if (nrow(strata) == 0) {
    stop(paste("No strata in query. Available stock area selections include:",unique(s1$stock_season)))
  }
  
  # Load strata
  strata <- map_strata(stock_name = stock_name,
                       stock_season = stock_season,
                       common_name = common_name,
                       strata = strata,
                       save_plot = F,
                       get_sf = T) %>%  
    methods::as("sf") %>% 
    sf::st_transform(st_crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) %>% 
    dplyr::select_at(.,vars(geometry, stock_season))
  
  
  # Load raster and convert to stars
  r <- loadRData(here::here("data-raw",r)) 
  raster::crs(r) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  r <- stars::st_as_stars(r)
  
  # Crop input raster to match strata
  out <- r[strata]
  
  return(out)

}
