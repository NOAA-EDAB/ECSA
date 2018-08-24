#' Downsample strata for masking 
#' 
#' This function converts NEFSC Bottom Trawl depth stata in "BTS_strata" shapefiles to a downsampled raster format,
#' which is necessary for masking target rasters containing ecosystem data. Primarily an interal function used by \code{stock_env()}
#' 
#'   
#' @param svspp Species code as specified in data/species_list.csv
#' @param season Season in which trawl survey occurred.
#' @param mask_type Specifies raster masking behavior. Can be one of "nes", "gom", "gbk", "sne", or "unit".
#' If mask is "unit", then returned time series reflect stock boundaries drawn from depth strata.
#' 
#' @return Returns a downsampled raster depicting area of interest. 
#' @export
#' 
#' @examples
#' resample_strat(svspp = 103, mask_type = "unit", season = "spring")


resample_strat <- function(svspp, season, mask_type){
  
  sea_stock_strata <- read.csv('data/seasonal_stock_strata.csv', stringsAsFactors = F)
  sps1 <- read.csv("data/species_list.csv", stringsAsFactors = F)
  all_stocks <- read.csv("data/all_strata.csv", stringsAsFactors = F)
  sp <- 1
  
  # choose season
  sps = sps1[sps1$season==season,]
  if (nrow(sps) < 1){stop("invalid season")}
  
  # choose for single species
  sps = sps[sps$svspp==svspp,]
  if (nrow(sps) < 1){stop("invalid svspp")}
  
  # choose for single species
  sps = sps[sps$stock_area==mask_type,]
  if (nrow(sps) < 1){stop("invalid mask type")}
  
  #filter season
  temp_strata = sea_stock_strata[sea_stock_strata$season==season,]
  
  #filter species
  temp_strata = temp_strata[temp_strata$sp==as.character(unlist(sps[sp,5])),]
  
  #filter stock area type and strata
  if (mask_type %in% unique(temp_strata$stock_area)){
    temp_strata = temp_strata[temp_strata$stock_area==as.character(unlist(sps[sp,7])),]
    temp_strata = temp_strata[temp_strata$strata%in%all_stocks$strata,]
  } else {
    nes <- unique(sea_stock_strata$strata)
  }
  

  
  #read in shape files
  strata <- rgdal::readOGR(dsn = 'data/strata_shapefiles', verbose = F)
  if (mask_type == 'nes'){
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