#function for pulling ocean temperature, salinity, and chlorophyll content given species stock strata.

stock_env <- function(variable, type, season,
                         svspp, mask_type, xlab,
                      ylab, ylim = NULL, plt = F){
  
  #filter steps--------------------------------------------------------------------------
  
  # just the spring species/stocks
  sps = sps[sps$season==season,]
  
  # choose for single species
  sps = sps[sps$svspp==svspp,]
  
  # choose for single species
  sps = sps[sps$stock_area==mask_type,]
  
  sp = 1  # for now, just point to the one record
  
  #get stock area if not NES-------------------------------------------------------------
  
  #create blank raster and merge into it
  r <- raster(ncol = 105, nrow = 90)
  extent(r) <-  extent(-75.95,-65.45,35.65,44.65)
  r@crs <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  stockmask.raster <- r

  #---------------------------------------------------------------------------------------
  
  if(sps[sp,7]!="nes"){
    #          print(sps[sp,7])
    stockmask.raster[]=NA
    
    #filter season
    temp_strata = sea_stock_strata[sea_stock_strata$season==season,]
    
    #filter species
    temp_strata = temp_strata[temp_strata$sp==as.character(unlist(sps[sp,5])),]
    
    #filter stock area type and strata
    temp_strata = temp_strata[temp_strata$stock_area==as.character(unlist(sps[sp,7])),]
    temp_strata = temp_strata[temp_strata$strata%in%df.stata_nums$stata_nums,]
    
    # stock specific blanking
    for (si in 1:nrow(temp_strata)){
      load(paste0("data/rast_masks/bts_",temp_strata[si,4],"_bmask.rdata"))
      stockmask.raster = merge(stockmask.raster,masked.raster)
    } # end for stock specific stock blanking
  } else {
    stockmask.raster[] <- NA
    for (si in 1:nrow(df.stata_nums)){
      load(paste0("data/rast_masks/bts_",df.stata_nums$stata_nums[si],"_bmask.rdata"))
      stockmask.raster = merge(stockmask.raster,masked.raster)
    } 
  }# end test for case not nes and build stock blanking

  
  #get bottom temp data and find mean for stock area---------------------------------------
  
  if (variable == "salinity"){
    variable <- "sal"
    indir = paste0("data/oi_",type,"_",variable,"_2018/",season,"_spdf/raster/")
  } else if (variable == "temperature"){
    variable <- "temp"
    indir = paste0("data/oi_",type,"_",variable,"_2018/",season,"_spdf/raster/")
  } else if (variable == "chlorophyll"){
    indir = "data/est_grid_version/"
  }
  
  #id raster files
  files = list.files(path=indir, pattern="RAST")
  
  #create null df to fill with results
  data = data.frame(array(NA,dim= c(length(files),4)))
  
  for(i in 1:length(files)){
    
    #load raster by year
    load(paste0(indir,files[i]))
    
    #get file information from title
    data[i,1] = as.numeric(substr(files[i],13,16))
    data[i,2] = as.numeric(substr(files[i],18,19))
    data[i,3] = as.numeric(substr(files[i],21,22))
    
    #trim to stock area
    masked.raster = masked.raster*stockmask.raster
    
    #find mean BT of stock area
    data[i,4] = cellStats(masked.raster, stat='mean', na.rm=TRUE)
  }
  
  
  if (variable == "chlorophyll" & season == "fall"){
    x <- data[data$X2==10,]$X1
    y.out <- data[data$X2 == 10,]$X4
  } else if (variable == "chlorophyll" & season == "spring"){
    x <- data[data$X2==4,]$X1
    y.out <- data[data$X2 == 4,]$X4
  } else if (variable == "temp" | variable == "sal"){
    x <- data$X1
    y.out <- data$X4
  }
  
  #plot------------------------------------------------------------------------------------
  if (plt){
    par(mar=c(5, 5, 4, 1))
    plot(x,y.out, pch=16,cex=2, xlab=xlab, ylab=ylab,
         cex.lab=1.75, cex.axis=1.5, ylim = ylim)
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dashed")
    points(x,y.out,pch=16,cex=2)
    lines(x,y.out, lwd=2)
    box(lwd=2)
  }
  
  #interpolate data if necessary-----------------------------------------------------------
  
  fdata = y.out
  filldata = fdata
  
  # fill missing at beginning
  fillc=1
  while(is.na(fdata[fillc+1])){fillc=fillc+1}
  if(fillc>0){filldata[1:fillc]=filldata[fillc+1]}
  
  # fill missing at end
  fillc=0
  lx = length(x)
  while(is.na(fdata[lx-fillc])){fillc=fillc+1}
  if(fillc>0){filldata[(lx-(fillc-1)):lx]=filldata[(lx-fillc)]}
  
  # fill missing in middle
  #x=data[seq(fweek,fweek+22),4]
  y.int=filldata
  ok <- complete.cases(x,y.int)
  nx <- x[ok]
  ny <- y.int[ok]
  xf=x[ok==FALSE]
  
  #interpolate
  lin  = interp1(nx, ny, xf, 'linear', extrap = TRUE)
  if (length(xf)>0){filldata[which(ok==FALSE)]=lin}
  
  #fill with new data
  y.int = filldata
  
  if(variable == "chlorophyll"){type <- ""}
  
  out <- data.frame(Var = paste(type,variable,season),
                    Time = x,
                    Value = y.out,
                    Species = svspp,
                    Season = season,
                    Stock_type = mask_type)
 

  out <- out[out$Time > 1968,]

   
  if (!plt){
    return(out)
  }
}

#this function plots 
# ocean_temp(type = "surface",season = "fall", svspp = 103, mask_type = "unit",
#    xlab = "Time",ylab = expression(paste("Bottom temperature ",degree,"C")),
#    ylim = NULL, plt = T)

#can return data.frame as well
# sf1 <- ocean_temp(type = "surface",season = "fall", svspp = 103, mask_type = "unit")
# ss1 <- ocean_temp(type = "surface",season = "spring", svspp = 103, mask_type = "unit")
# 
# bf1 <- ocean_temp(type = "bottom",season = "fall", svspp = 103, mask_type = "unit")
# bs1 <- ocean_temp


