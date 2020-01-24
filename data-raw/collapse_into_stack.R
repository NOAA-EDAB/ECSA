library(sf)
library(raster)
library(stars)
library(stringr)
library(dplyr)
folder_path = tmp_dir
#A function to collapse many RasterLayers (saved as .RData files) within a directory to a RasterStack. 
#The argument fname_regex allows for filtering file names according to some regex. fname_regex passes
#all files by default (i.e. .*). 
collapse_into_stack <- function(folder_path, fname_regex = ".*"){
  
  message(folder_path)  
  
  #Select files that meet the relevant regex criteria
  p <- list.files(folder_path)[stringr::str_detect(list.files(folder_path), fname_regex)]
  
  #Load each files in the directory and put it into a RasterStack
  out <- raster::stack()
  for (i in 1:length(p)){

    f <- p[i]
    
    if (stringr::str_detect(f, "\\.RData|\\.rdata|\\.Rdata")){
      
      fname <- stringr::str_split(f, "\\.RData|\\.rdata|\\.Rdata")[[1]][1]
      obj <- loadRData(file.path(folder_path, f))
      
      
      assign("out", raster::stack(out,obj))
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



#NOTE: raw.dir must be the folder containing individual raster files

#Chlorophyll
chlorophyll_conc <- collapse_into_stack(folder_path = file.path(raw.dir,"chlorophyll concentration"))


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
spring_blabas_occ_prob <- collapse_into_stack(file.path(raw.dir, "spring-occupancy/spring"),
                    fname_regex = "(?=.*blabas)(?=.*PA)")
save(spring_blabas_occ_prob, file = "spring_blabas_occ_prob.rdata")

spring_monkfh_occ_prob <- collapse_into_stack(file.path(raw.dir, "spring-occupancy/spring"),
                                              fname_regex = "(?=.*monkfh)(?=.*PA)")
save(spring_monkfh_occ_prob, file = "spring_monkfh_occ_prob.rdata")

spring_scupzz_occ_prob <- collapse_into_stack(file.path(raw.dir, "spring-occupancy/spring"),
                                              fname_regex = "(?=.*scupzz)(?=.*PA)")
save(spring_scupzz_occ_prob, file = "spring_scupzz_occ_prob.rdata")

spring_bluefi_occ_prob <- collapse_into_stack(file.path(raw.dir, "spring-occupancy/spring"),
                                              fname_regex = "(?=.*bluefi)(?=.*PA)")
save(spring_bluefi_occ_prob, file = "spring_bluefi_occ_prob.rdata")

fall_blabas_occ_prob <- collapse_into_stack(file.path(raw.dir, "fall-occupancy/fall"),
                                              fname_regex = "(?=.*blabas)(?=.*PA)")
save(fall_blabas_occ_prob, file = "fall_blabas_occ_prob.rdata")

fall_monkfh_occ_prob <- collapse_into_stack(file.path(raw.dir, "fall-occupancy/fall"),
                                              fname_regex = "(?=.*monkfh)(?=.*PA)")
save(fall_monkfh_occ_prob, file = "fall_monkfh_occ_prob.rdata")

fall_scupzz_occ_prob <- collapse_into_stack(file.path(raw.dir, "fall-occupancy/fall"),
                                              fname_regex = "(?=.*scupzz)(?=.*PA)")
save(fall_scupzz_occ_prob, file = "fall_scupzz_occ_prob.rdata")

fall_bluefi_occ_prob <- collapse_into_stack(file.path(raw.dir, "fall-occupancy/fall"),
                                              fname_regex = "(?=.*bluefi)(?=.*PA)")
save(fall_bluefi_occ_prob, file = "fall_bluefi_occ_prob.rdata")

library(dplyr)
library(googledrive)

# stock_list <- read.csv("data/stock_list.csv", stringsAsFactors = FALSE)
# stock_list <- unique(stock_list$species_code)

drive_ids <- dplyr::bind_rows(googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1_V3mHOc3M1DjLdjCrc7saW3-LvTiJPDd?usp=sharing"),
                                    recursive = TRUE, n_max = Inf),
                              googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1ZYJk7GtkGLTUOukCal8KIwl_rYXkYJWZ?usp=sharing"),
                                                    recursive = TRUE, n_max = Inf)) %>% 
  dplyr::select(-drive_resource) %>% 
  data.frame

# tt <- googledrive::drive_get(googledrive::as_id("https://drive.google.com/drive/folders/1ZYJk7GtkGLTUOukCal8KIwl_rYXkYJWZ?usp=sharing"))
# tt <- googledrive::drive_ls(path = "~/ecsa/spring")
# 
# td <- drive_ls(tt, n_max = Inf)
# 

drive_list <- drive_ids %>%
  dplyr::filter(stringr::str_detect(name, pattern = "RAST_(?=.*PA)")) %>%
  tidyr::separate(col = name, into = c("stock_name", "A", "B", "C", "season", "D", "E", "F"), sep = "\\.", remove = FALSE) %>% 
  tidyr::separate(col = stock_name, into = c("G", "species_code", "year"), sep = "_") %>% 
  dplyr::select(-A, -B, -C, -D, -E, -F, -G) %>% 
  dplyr::arrange(species_code, season, year) %>% 
  dplyr::distinct(.keep_all = TRUE)

drive_list %>% 
  dplyr::filter(!species_code %in% c("blabas", "bluefi", "monkfh", "scupzz")) %>%
  dplyr::mutate(path = paste0(path.expand(tmp_dir), "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))


drive_list %>% 
  # dplyr::filter(species_code == "atlmac") %>% 
  dplyr::filter(!species_code %in% c("blabas", "bluefi", "monkfh", "scupzz")) %>%
  select(species_code, season) %>% 
  distinct(.keep_all = TRUE) %>% 
  purrr::pmap(function(season, species_code, ...){
    x = collapse_into_stack(tmp_dir, fname_regex = sprintf("(?=.*%s)(?=.*%s)", species_code, season))
    fname = sprintf("data-raw/%s_%s_occ_prob", season, species_code)
    assign(fname, x)
    save(list = fname, file = sprintf("%s.rdata", fname))
  }
  )



fall_atlher_occ_prob <- collapse_into_stack(tmp_dir,
                                            fname_regex = sprintf("(?=.*atlmac)(?=.*%s)", "fall"))


