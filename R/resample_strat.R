# internal function to resample high res stock strata
# to match strata masks used in analyses

#returns a raster of the down-sampled tock strata

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
  strata <- readOGR(dsn = 'data/strata_shapefiles', verbose = F)
  if (mask_type == 'nes'){
    stock_strata <- strata[strata@data$STRATA %in% nes,]
  } else {
    stock_strata <- strata[strata@data$STRATA %in% temp_strata$strata,]
  }
  
  #create empty raster
  r1 <- raster()
  e <- extent(-75.950, -65.450, 35.650, 44.650)
  extent(r1) <- e
  
  #fill with strate
  r1 <- rasterize(stock_strata, r1, field = 1, fun = mean)
  crs(r1) <- NA
  
  #create raster to resample with
  r2 <- raster(nrow = 90, ncol = 105)
  extent(r2) <- e
  crs(r2) <- NA
  
  #resample high res raster to match data
  r.new <- raster::resample(r1, r2, method="bilinear")
  r.new[is.finite(r.new)] <- 1
  
  return(r.new)
  
}


#sumflo_spring <- match_strat(svspp = 103, mask_type = "unit", season = "spring")
#sumflo_fall <- match_strat(svspp = 103, mask_type = "unit", season = "fall")



