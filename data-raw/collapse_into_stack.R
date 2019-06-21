library(sf)
library(raster)
library(stars)
library(stringr)
library(dplyr)

#A function to collapse many RasterLayers (saved as .RData files) within a directory to a RasterStack
collapse_into_stack <- function(folder_path){
  p <- folder_path
  message(p)  
  
  
  #Load each files in the directory and put it into a RasterStack
  out <- raster::stack()
  for (i in 1:length(list.files(p))){

    f <- list.files(p)[i]
    
    if (stringr::str_detect(f, "\\.RData|\\.rdata|\\.Rdata")){
      
      fname <- stringr::str_split(f, "\\.RData|\\.rdata|\\.Rdata")[[1]][1]
      obj <- loadRData(file.path(p,f))
      
      
      assign("out",stack(out,obj))
      names(out)[i] <- fname
      message(fname)
    } else {
      message(paste0(f, " is not a .RData file"))
    }
  
  }
  
  
  return(out)
}
  
# A handy function to rename .Rdata files 
# (https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file)
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}



biomass_fall <- collapse_into_stack(folder_path = file.path(raw.dir,"biomass/fall"))
biomass_spring <- collapse_into_stack(folder_path = file.path(raw.dir,"biomass/spring"))
# save(biomass_fall, file = paste0(raw.dir,"/biomass_fall.rdata"))
# save(biomass_spring, file = paste0(raw.dir,"/biomass_spring.rdata"))

chlorophyll_conc <- collapse_into_stack(folder_path = file.path(raw.dir,"chlorophyll concentration"))
# save(chlorophyll_conc, file = paste0(raw.dir,"/chlorophyll_conc.rdata"))

raw.dir <- "c:/users/sean.hardison/downloads/ecsa menhaden"

#Zooplankton
for (k in c("spring","fall")){
  for (i in c("rasters_1yr")){
    print(paste(k,i))
    # suppressMessages(
      assign(paste0("zoo_",k,"_",i),
             collapse_into_stack(folder_path = file.path(raw.dir, "zooplankton", k,i)))
      # )
    # save(list = paste0("zoo_",k,"_",i), file = file.path(raw.dir,paste0("zoo_",k,"_",i,".rdata")))
  }
}

save(file = "~/git/ecsa/data-raw/zoo")

#Ocean temperature
for (k in c("bot_temp","surf_temp")){
  for (i in c("fall_spdf","spring_spdf")){
    print(paste(k,i))
    # suppressMessages(
    assign(paste0(k,"_",i),
           collapse_into_stack(folder_path = file.path(raw.dir, "temperature", k,i,"rasters")))
    # )
    # save(list = paste0(k,"_",i), file = file.path(raw.dir,paste0(k,"_",i,".rdata")))
  }
}

save(zoo_spring_rasters_1yr, file = "~/zoo_spring_rasters_1yr.rdata")

#Ocean salinity
for (k in c("bot_sal","surf_sal")){
  for (i in c("fall_spdf","spring_spdf")){
    print(paste(k,i))
    # suppressMessages(
    assign(paste0(k,"_",i),
           collapse_into_stack(folder_path = file.path(raw.dir, "salinity", k,i,"rasters")))
    # )
    save(list = paste0(k,"_",i), file = file.path(raw.dir,paste0(k,"_",i,".rdata")))
  }
}

#Occurrence probability
occurrence_prob_fall <- collapse_into_stack(folder_path = file.path(raw.dir,"occurrence probability/fall"))
occurrence_prob_spring <- collapse_into_stack(folder_path = file.path(raw.dir,"occurrence probability/spring"))
# save(occurrence_prob_fall, file = paste0(raw.dir,"/occurrence_prob_fall.rdata"))
# save(occurrence_prob_spring, file = paste0(raw.dir,"/occurrence_prob_spring.rdata"))


