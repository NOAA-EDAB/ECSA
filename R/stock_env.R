# function for pulling ocean temperature, salinity, chlorophyll, and zooplankton
# content given species stock strata.

# variable - Can be "chlorophyll", "temperature", "salinity", "zooplankton".
# svspp - Input svspp code for species of interest.
# type - Specific to temperature and salinity variables. Either "bottom" or "surface".
# season - Either "spring" or "fall".
# genus - Specific to zooplankton. One of "centropages", "temora", or "pseudocalanus".
# mask_type - Specifies raster masking behavior. Can be either "nes" to use the full
# shelf, or "unit" to filter by species-specific stock area.
# plt - Plots output rather than returning a data frame of results. 
# ylab, xlab - Plot labels if plt = T
# ylim - Vector in the form of c(min,max) to specifiy y limits if plt = T




stock_env <- function(variable, type = NULL, season, genus = NULL,
                         svspp, mask_type, xlab,
                      ylab, ylim = NULL, plt = F){
  
  if(!is.null(type) & !variable %in% c("salinity","temperature")){
    stop('type only applicable for variables "salinity" and "temperature"
         as type = "bottom" or type = "surface"')
  } 
  
  if(!is.null(genus) & variable != "zooplankton"){
    stop('genus only applicable for variable "zooplankton"')
  } 
  
  #filter steps--------------------------------------------------------------------------
  
  # choose season
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

  
  #get bottom temp data and find mean for stock area--------------------------------------
  
  if (variable == "salinity"){
    indir = paste0("data/oi_",type,"_sal_2018/",season,"_spdf/raster/")
  } else if (variable == "temperature"){
    indir = paste0("data/oi_",type,"_temp_2018/",season,"_spdf/raster/")
  } else if (variable == "chlorophyll"){
    indir = "data/est_grid_version/"
  } else if (variable == "zooplankton"){
    indir <- paste0("data/zoo/",season,"/",genus,"/")
  }
  
  #id raster files
  files = list.files(path=indir, pattern="RAST")
  if (variable == "chlorophyll"){
    if (season == "fall"){
      files <- files[grepl('\\d{4}\\.10',files)]
    } else if (season == "spring"){
      files <- files[grepl('\\d{4}\\.04',files)]
    }
    
  }
  
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
  
    x <- data$X1
    y.out <- data$X4
    
    # remove 
    if (variable == "zooplankton"){
      if (season == "spring"){
        y.out[x %in% c(1989, 1990, 1991, 1994)] <- NA
      } else if (season == "fall")
        y.out[x %in% c(1989, 1990, 1992)] <- NA
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
  
  if(variable == "chlorophyll"){
    type <- ""
  } else if (variable == "zooplankton"){
      type <- genus
      variable <- "zoo"
  }
  
  
  out <- data.frame(Var = paste(paste(type,variable),season),
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

#USAGE --------------------------------------------------------------------------

# sf1 <- stock_env(variable = "salinity",type = "surface",
#                  season = "spring", svspp = svspp, mask_type = "unit")
# ss1 <- stock_env(variable = "salinity",type = "surface",
#                  season = "fall", svspp = svspp, mask_type = "unit")
# 
# bf1 <- stock_env(variable = "salinity",type = "bottom",
#                  season = "spring", svspp = svspp, mask_type = "unit")
# bs1 <- stock_env(variable = "salinity",type = "bottom",
#                  season = "fall", svspp = svspp, mask_type = "unit")
# 
# Plotting example ---------------------------------------------------------------
# surface <- rbind(sf1, ss1)
# bottom <- rbind(bf1, bs1)
# 
# xmin <- rbind(min(surface$Time),min(bottom$Time))
# xmin <- min(xmin)
# 
# #Y scales can be adjusted by adding 'scales' argument to facet_wrap.
# library(ggplot2);library(gridExtra)
# s_plt <- ggplot(data = surface, aes(x = Time, y = Value)) +
#   ylab("Surface Salinity (PSU)") +
#   xlab("") +
#   xlim(xmin, NA) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(Season ~., nrow = 1) +
#   theme_bw() +
#   theme(plot.title = element_blank(),
#         strip.background = element_blank(),
#         strip.text.x = element_blank()) +
#   annotate("text", label = c("A","B"), x = xmin, y = Inf, vjust = 1.5, size = 5)
# 
# b_plt <- ggplot(data = bottom, aes(x = Time, y = Value)) +
#   ylab("Bottom Salinity (PSU)") +
#   xlab("Year") +
#   xlim(xmin, NA) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(Season ~., nrow = 1) +
#   theme_bw() +
#   theme(plot.title = element_blank(),
#         strip.background = element_blank(),
#         strip.text.x = element_blank()) +
#   annotate("text", label = c("C","D"), x = xmin, y = Inf, vjust = 1.5, size = 5)
# 
# grid.arrange(s_plt, b_plt, nrow = 2)
# 

#Do not include "type" when calling CHL or Zoolplankton:

# CHL-----------------------------------------------------------------------------
# cs1 <- stock_env(variable = "chlorophyll",
#                  season = "spring", svspp = svspp, mask_type = "unit")

# Zooplankton --------------------------------------------------------------------
# ct1 <- stock_env(variable = "zooplankton", genus = "centropages",
#                  season = "spring", svspp = 103, mask_type = "unit")
# 
# ct2 <- stock_env(variable = "zooplankton", genus = "centropages",
#                  season = "fall", svspp = 103, mask_type = "unit")
# 
# t1 <- stock_env(variable = "zooplankton", genus = "temora",
#                  season = "spring", svspp = 103, mask_type = "unit")
# 
# t2 <- stock_env(variable = "zooplankton", genus = "temora",
#                  season = "fall", svspp = 103, mask_type = "unit")
# 
# ps1 <- stock_env(variable = "zooplankton", genus = "pseudocalanus",
#                 season = "spring", svspp = 103, mask_type = "unit")
# 
# ps2 <- stock_env(variable = "zooplankton", genus = "pseudocalanus",
#                 season = "fall", svspp = 103, mask_type = "unit")
# Another plot -------------------------------------------------------------------
# xmin <- min(ct1$Time,ct2$Time,t1$Time,t2$Time,ps1$Time,ps2$Time)
# zoo <- rbind(ct1, ct2, t1, t2, ps1, ps2)
# library(ggplot2);library(gridExtra)
# ggplot(data = zoo, aes(x = Time, y = Value)) +
#   ylab("Abundance log num m^-3") +
#   xlab("") +
#   xlim(xmin, NA) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(Var ~ Season, nrow = 3, ncol = 2, scale = "free_y") +
#   theme_bw() +
#   theme(plot.title = element_blank(),
#         strip.background = element_blank(),
#         strip.text.x = element_blank()) +
#   annotate("text", label = c("A","B","C","D","E","F"),
#             x = xmin, y = Inf, vjust = 1.5, size = 5)


