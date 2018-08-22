#'Compile ECSA Data.
#'
#'This function aggregates data for stock-specific ecosystem context reporting. Data that can be 
#'accessed include surface and bottom temperature and salinity, chlorophyll concentration, 
#'zooplankton abundance, and occupancy probability for the US Northeast Shelf. 
#'
#'Data for this function are currently limited to american lobster (svspp = 301), summer flounder
#'(svspp = 103), and jonah crab (svspp = 312). Occupancy habitat is limited to summer flounder only.
#' 
#' 
#' @param Variable Can be "chlorophyll", "temperature", "salinity", "zooplankton", or "occupancy".
#' @param svspp Input svspp code for species of interest.
#' @param type Specific to temperature and salinity variables. Either "bottom" or "surface".
#' @param season Either "spring" or "fall".
#' @param genus Specific to zooplankton. Can be one of "centropages", "temora", or "pseudocalanus".
#' @param mask_type Specifies raster masking behavior. Can be one of "nes", "gom", "gbk", "sne", or "unit".
#' If mask is "unit", then returned time series reflect stock boundaries drawn from depth strata.
#' @param plt Plots output rather than returning a data frame of results. 
#' @param ylab Plot y label if plt = T.
#' @param xlab Plot labels if plt = T.
#' @param ylim Vector in the form of c(min,max) to specifiy y limits if plt = T.
#' @param interpo Logical. If missing values should be interpolated linearly. Current not functional. 
#' 
#' @return If plt = F, a data frame containing time series of compiled data is returned.
#' 
#' @examples
#' USAGE --------------------------------------------------------------------------
#'
#' sf1 <- stock_env(variable = "salinity",type = "surface",
#'                   season = "spring", svspp = 103, mask_type = "unit")
#' ss1 <- stock_env(variable = "salinity",type = "surface",
#'                  season = "fall", svspp = 103, mask_type = "unit")
#' 
#' bf1 <- stock_env(variable = "salinity",type = "bottom",
#'                  season = "spring", svspp = 103, mask_type = "unit")
#' bs1 <- stock_env(variable = "salinity",type = "bottom",
#'                  season = "fall", svspp = 103, mask_type = "unit")
#'                  
#' Plotting example ---------------------------------------------------------------
#' surface <- rbind(sf1, ss1)
#' bottom <- rbind(bf1, bs1)
#' 
#' xmin <- rbind(min(surface$Time),min(bottom$Time))
#' xmin <- min(xmin)
#' 
#' Y scales can be adjusted by adding 'scales' argument to facet_wrap.
#' library(ggplot2);library(gridExtra)
#' s_plt <- ggplot(data = surface, aes(x = Time, y = Value)) +
#'   ylab("Surface Salinity (PSU)") +
#'   xlab("") +
#'   xlim(xmin, NA) +
#'   geom_line() +
#'   geom_point() +
#'   facet_wrap(Season ~., nrow = 1, scale = 'free_y') +
#'   theme_bw() +
#'   theme(plot.title = element_blank(),
#'         strip.background = element_blank(),
#'         strip.text.x = element_blank()) +
#'   annotate("text", label = c("A","B"), x = xmin, y = Inf, vjust = 1.5, size = 5)
#' 
#' b_plt <- ggplot(data = bottom, aes(x = Time, y = Value)) +
#'   ylab("Bottom Salinity (PSU)") +
#'   xlab("Year") +
#'   xlim(xmin, NA) +
#'   geom_line() +
#'   geom_point() +
#'   facet_wrap(Season ~., nrow = 1, scale = 'free_y') +
#'   theme_bw() +
#'   theme(plot.title = element_blank(),
#'         strip.background = element_blank(),
#'         strip.text.x = element_blank()) +
#'   annotate("text", label = c("C","D"), x = xmin, y = Inf, vjust = 1.5, size = 5)
#' 
#' grid.arrange(s_plt, b_plt, nrow = 2)
#' 
#' Do not include "type" when calling CHL, zooplankton, or occupancy:
#' CHL-----------------------------------------------------------------------------
#' cs1 <- stock_env(variable = "chlorophyll",
#'                  season = "spring", svspp = svspp, mask_type = "unit")
#' cf1 <- stock_env(variable = "chlorophyll",
#'                  season = "fall", svspp = svspp, mask_type = "unit")
#' chl <- rbind(cs1, cf1)
#' xmin <- rbind(min(chl$Time),min(chl$Time))
#' xmin <- min(xmin)
#' ggplot(data = chl, aes(x = Time, y = Value)) +
#'     ylab("Chlorophyll mg m^-3") +
#'     xlab("") +
#'     xlim(xmin, NA) +
#'     geom_line() +
#'     geom_point() +
#'     facet_wrap(Season ~., nrow = 1, scale = 'free_y') +
#'     theme_bw() +
#'     theme(plot.title = element_blank(),
#'         strip.background = element_blank(),
#'         strip.text.x = element_blank()) +
#'     annotate("text", label = c("A","B"), x = xmin, y = Inf, vjust = 1.5, size = 5)
#'     
#' Zooplankton --------------------------------------------------------------------
#' ct1 <- stock_env(variable = "zooplankton", genus = "centropages",
#'                  season = "spring", svspp = 103, mask_type = "unit")
#' 
#' ct2 <- stock_env(variable = "zooplankton", genus = "centropages",
#'                  season = "fall", svspp = 103, mask_type = "unit")
#' 
#' t1 <- stock_env(variable = "zooplankton", genus = "temora",
#'                  season = "spring", svspp = 103, mask_type = "unit")
#' 
#' t2 <- stock_env(variable = "zooplankton", genus = "temora",
#'                  season = "fall", svspp = 103, mask_type = "unit")
#' 
#' ps1 <- stock_env(variable = "zooplankton", genus = "pseudocalanus",
#'                 season = "spring", svspp = 103, mask_type = "unit")
#' 
#' ps2 <- stock_env(variable = "zooplankton", genus = "pseudocalanus",
#'                 season = "fall", svspp = 103, mask_type = "unit")


stock_env <- function(variable, type = NULL, season, genus = NULL,
                         svspp, mask_type, xlab,interpo = F,
                      ylab, ylim = NULL, plt = F){
  
  if(!is.null(type) & !variable %in% c("salinity","temperature")){
    stop('type only applicable for variables "salinity" and "temperature"
         as type = "bottom" or type = "surface"')
  } 
  
  if(!is.null(genus) & variable != "zooplankton"){
    stop('genus only applicable for variable "zooplankton"')
  } 

  #get compiled down-sampled raster of chosen strata from shapefile. 
  stockmask.raster <- resample_strat(svspp = svspp, mask_type = mask_type, season = season)

  #get bottom temp data and find mean for stock area--------------------------------------
  
  indir <- "data/gridded"
  
  if (variable == "salinity"){
    load(file.path(indir, paste0("sal_",type,"_",season,"_spdf.rdata")))
  } else if (variable == "temperature"){
    load(file.path(indir, paste0("temp_",type,"_",season,"_spdf.rdata")))
  } else if (variable == "chlorophyll"){
    load(file.path(indir, paste0("chl_",season,"_1997-2018.rdata")))
  } else if (variable == "zooplankton"){
    load(file.path(indir, paste0(genus,"_",season,"_zoo_1977-2016.rdata")))
  } else if (variable == "occupancy"){
    load(file.path(indir, paste0("sumflo_occupancy_PA_",season,".rdata")))
    warning("As of 8/21, occupancy probability data are only available for summer flounder.")
  }
  

  #create null df to fill with results
  data = data.frame(array(NA,dim= c(nlayers(ecsa_dat),4)))
  
  #loops through layers in raster brick
  for(i in 1:nlayers(ecsa_dat)){
    #load raster by year
    
    #get file information from title
    layer_id <- str_extract(names(ecsa_dat)[[i]], "\\d.*")
    layer_id <- str_split(layer_id, "_")
    data[i,1] <- layer_id[[1]][[1]]
    data[i,2] <- layer_id[[1]][[2]]
    data[i,3] <- layer_id[[1]][[3]]
  
    #trim to stock area
    masked.raster = ecsa_dat[[i]]*stockmask.raster
    
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
  if (interpo){
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
  }
  
  
  if(variable == "chlorophyll"){
    type <- ""
  } else if (variable == "zooplankton"){
      type <- genus
      variable <- "zoo"
  }
  
  
  out <- data.frame(Var = paste(paste(type,variable),season),
                    Time = as.numeric(x),
                    Value = y.out,
                    Species = svspp,
                    Season = season,
                    Stock_type = mask_type)
 

  out <- out[out$Time > 1968,]

   
  if (!plt){
    return(out)
  }
}