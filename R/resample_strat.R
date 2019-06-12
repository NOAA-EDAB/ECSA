#' Downsample strata for masking 
#' 
#' This function converts NEFSC Bottom Trawl depth stata in "BTS_strata" shapefiles to a downsampled raster format,
#' which is necessary for masking target rasters containing ecosystem data. See the file "seasonal_stock_strata.csv"
#' for choosing species and stock areas of interest. 
#' 
#'   
#' @param svspp Species code as specified in data/species_list.csv
#' @param season Season in which trawl survey occurred.
#' @param mask_type Specifies masking behavior for specific stock areas. Can be one of "unit", "gbk",
#'  "gom", "south", "north", "snemab", "gbkgom", "ccgom", or "sne".  
#' 
#' 
#' @return Returns a downsampled raster depicting stock area of interest. 
#' @export
#' 
#' @examples
#' resample_strat(svspp = 103, mask_type = "unit", season = "spring")


resample_strat <- function(svspp, season, mask_type){
  
  sea_stock_strata <- read.csv('data/seasonal_stock_strata.csv', stringsAsFactors = F)

  #Filter strata
  temp_strata <- sea_stock_strata %>% filter(season_ == season, 
                                                SVSPP == svspp,
                                                stock_area == mask_type)
  #Break if no matching variables
  if (nrow(temp_strata) == 0){
    stop('Seasonal stock strata, species code, and stock area ("mask type") must correspond.\n See seasonal_stock_strata.csv for complete listing.')
  }
  
  #read in shape file
  strata <- rgdal::readOGR(dsn = 'data/strata_shapefiles', verbose = F)
  
  if (mask_type == 'nes'){
    nes <- unique(sea_stock_strata$strata)
    stock_strata <- strata[strata@data$STRATA %in% nes,]
  } else {
    stock_strata <- strata[strata@data$STRATA %in% temp_strata$strata,]
  }
  
  #create empty raster
  r1 <- raster::raster()
  e <- raster::extent(-75.950, -65.450, 35.650, 44.650)
  raster::extent(r1) <- e
  
  #fill with strate
  r1 <- raster::rasterize(stock_strata, r1, field = 1, fun = mean)
  raster::crs(r1) <- NA
  
  #create raster to resample with
  r2 <- raster::raster(nrow = 90, ncol = 105)
  raster::extent(r2) <- e
  raster::crs(r2) <- NA
  
  #resample high res raster to match data
  r.new <- raster::resample(r1, r2, method="bilinear")
  r.new[is.finite(r.new)] <- 1
  
  return(r.new)
  
}
