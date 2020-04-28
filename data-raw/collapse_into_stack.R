library(sf)
library(raster)
library(stars)
library(stringr)
library(dplyr)
library(googledrive)

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


###### Chlorophyll ##############################################################

chl_drive_ids <- dplyr::bind_rows(googledrive::drive_ls(
  googledrive::as_id("https://drive.google.com/drive/u/1/folders/1A0r46TZc074HV181inIUY3uCg1piewUh"),
  recursive = TRUE, n_max = Inf)) %>% 
  dplyr::select(-drive_resource) %>% 
  data.frame

chl_drive_list <- chl_drive_ids %>%
  dplyr::filter(stringr::str_detect(name, pattern = "RAST_(?=.*CHL)")) %>%
  tidyr::separate(col = name, into = c("year", "Month", "B", "C", "chl",  "D", "E", "F"), sep = "\\.", remove = FALSE) %>% 
  mutate(Month = as.numeric(Month), 
         season = ifelse(Month > 6, "fall", "spring")) %>%  
  tidyr::separate(col = year, into = c("G", "H", "year"), sep = "_") %>% 
  dplyr::select( -B, -C, -D, -E, -F, -G, -H, -Month) %>% 
  dplyr::arrange(chl, year, season) %>% 
  dplyr::distinct(.keep_all = TRUE)

chl_spring_drive_list <- chl_drive_list %>%
  dplyr::filter(season == "spring") %>% 
  dplyr::mutate(path = paste0(path.expand(file.path(here::here("temp/chl/spring"))),  "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))

chl_fall_drive_list <- chl_drive_list %>%
  dplyr::filter(season == "fall") %>% 
  dplyr::mutate(path = paste0(path.expand(file.path(here::here("temp/chl/fall"))),  "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))

chlorophyll_conc_spring <- collapse_into_stack(file.path(here::here("temp/chl/spring")))
chlorophyll_conc_fall <- collapse_into_stack(file.path(here::here("temp/chl/fall")))

save(chlorophyll_conc_spring, file = here::here("data-raw/chlorophyll_conc_spring.rdata"))
save(chlorophyll_conc_fall, file = here::here("data-raw/chlorophyll_conc_fall.rdata"))

###################################################################################################

############## Temperature #############################################

temp_drive_ids <- dplyr::bind_rows(googledrive::drive_ls(
  googledrive::as_id("https://drive.google.com/drive/u/1/folders/1qxuUQdpPZWgpDPPDLdB_2MOTMAk9aMq2"),
  recursive = TRUE, n_max = Inf)) %>% 
  dplyr::select(-drive_resource) %>% 
  data.frame


temp_drive_list <- temp_drive_ids %>%
  dplyr::filter(stringr::str_detect(name, pattern = "RAST_(?=.*TEMP)")) %>%
  tidyr::separate(col = name, into = c("year", "Month", "B", "position", "temp",  "D", "E", "F"), sep = "\\.", remove = FALSE) %>% 
  mutate(Month = as.numeric(Month), 
         season = ifelse(Month > 6, "fall", "spring")) %>%  
  tidyr::separate(col = year, into = c("G", "H", "year"), sep = "_") %>% 
  dplyr::select( -B, -D, -E, -F, -G, -H, -Month) %>% 
  dplyr::arrange(temp, year, season, position) %>% 
  dplyr::distinct(.keep_all = TRUE)


temp_spring_bottom_drive_list <- temp_drive_list %>%
  dplyr::filter(season == "spring" & position == "BT") %>% 
  dplyr::mutate(path = paste0(path.expand(file.path(here::here("temp/temperature/spring/BT"))),  "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))

temp_spring_surface_drive_list <- temp_drive_list %>%
  dplyr::filter(season == "spring" & position == "ST") %>% 
  dplyr::mutate(path = paste0(path.expand(file.path(here::here("temp/temperature/spring/ST"))),  "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))

temp_fall_bottom_drive_list <- temp_drive_list %>%
  dplyr::filter(season == "fall" & position == "BT") %>% 
  dplyr::mutate(path = paste0(path.expand(file.path(here::here("temp/temperature/fall/BT"))),  "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))

temp_fall_surface_drive_list <- temp_drive_list %>%
  dplyr::filter(season == "fall" & position == "ST") %>% 
  dplyr::mutate(path = paste0(path.expand(file.path(here::here("temp/temperature/fall/ST"))),  "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))

temperature_spring_surface <- collapse_into_stack(file.path(here::here("temp/temperature/spring/ST")))
temperature_spring_bottom <- collapse_into_stack(file.path(here::here("temp/temperature/spring/BT")))
temperature_fall_surface <- collapse_into_stack(file.path(here::here("temp/temperature/fall/ST")))
temperature_fall_bottom <- collapse_into_stack(file.path(here::here("temp/temperature/fall/BT")))


save(temperature_spring_surface, file = here::here("data-raw/temperature_spring_surface.rdata"))
save(temperature_spring_bottom, file = here::here("data-raw/temperature_spring_bottom.rdata"))
save(temperature_fall_surface, file = here::here("data-raw/temperature_fall_surface.rdata"))
save(temperature_fall_bottom, file = here::here("data-raw/temperature_fall_bottom.rdata"))



##############################################################################################################

############## Salinity #############################################
 
temp_drive_ids <- dplyr::bind_rows(googledrive::drive_ls(
  googledrive::as_id("https://drive.google.com/drive/u/1/folders/1qxuUQdpPZWgpDPPDLdB_2MOTMAk9aMq2"),
  recursive = TRUE, n_max = Inf)) %>% 
  dplyr::select(-drive_resource) %>% 
  data.frame


temp_drive_list <- temp_drive_ids %>%
  dplyr::filter(stringr::str_detect(name, pattern = "RAST_(?=.*TEMP)")) %>%
  tidyr::separate(col = name, into = c("year", "Month", "B", "position", "temp",  "D", "E", "F"), sep = "\\.", remove = FALSE) %>% 
  mutate(Month = as.numeric(Month), 
         season = ifelse(Month > 6, "fall", "spring")) %>%  
  tidyr::separate(col = year, into = c("G", "H", "year"), sep = "_") %>% 
  dplyr::select( -B, -D, -E, -F, -G, -H, -Month) %>% 
  dplyr::arrange(temp, year, season, position) %>% 
  dplyr::distinct(.keep_all = TRUE)


temp_spring_bottom_drive_list <- temp_drive_list %>%
  dplyr::filter(season == "spring" & position == "BS") %>% 
  dplyr::mutate(path = paste0(path.expand(file.path(here::here("temp/salnity/spring/BS"))),  "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))

temp_spring_surface_drive_list <- temp_drive_list %>%
  dplyr::filter(season == "spring" & position == "SS") %>% 
  dplyr::mutate(path = paste0(path.expand(file.path(here::here("temp/salinity/spring/SS"))),  "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))

temp_fall_bottom_drive_list <- temp_drive_list %>%
  dplyr::filter(season == "fall" & position == "BS") %>% 
  dplyr::mutate(path = paste0(path.expand(file.path(here::here("temp/salinity/fall/BS"))),  "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))

temp_fall_surface_drive_list <- temp_drive_list %>%
  dplyr::filter(season == "fall" & position == "SS") %>% 
  dplyr::mutate(path = paste0(path.expand(file.path(here::here("temp/salinity/fall/SS"))),  "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))

temperature_spring_surface <- collapse_into_stack(file.path(here::here("temp/salinity/spring/SS")))
temperature_spring_bottom <- collapse_into_stack(file.path(here::here("temp//salinity/spring/BS")))
temperature_fall_surface <- collapse_into_stack(file.path(here::here("temp//salinity/fall/SS")))
temperature_fall_bottom <- collapse_into_stack(file.path(here::here("temp//salinity/fall/BS")))


save(temperature_spring_surface, file = here::here("data-raw//salinity_spring_surface.rdata"))
save(temperature_spring_bottom, file = here::here("data-raw//salinity_spring_bottom.rdata"))
save(temperature_fall_surface, file = here::here("data-raw//salinity_fall_surface.rdata"))
save(temperature_fall_bottom, file = here::here("data-raw//salinity_fall_bottom.rdata"))

#######################################################################################################
















#Zooplankton
for (k in c("spring","fall")){ ## Zooplankton unchanged in the last year so did not rerun data
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

# #Ocean temperature
# for (k in c("bot_temp","surf_temp")){
#   for (i in c("fall_spdf","spring_spdf")){
#     print(paste(k,i))
#     # suppressMessages(
#     assign(paste0(k,"_",i),
#            collapse_into_stack(folder_path = file.path(raw.dir, "temperature", k,i,"rasters")))
#     # )
#     # save(list = paste0(k,"_",i), file = file.path(raw.dir,paste0(k,"_",i,".rdata")))
#   }
# }
# 
# save(zoo_spring_rasters_1yr, file = "~/zoo_spring_rasters_1yr.rdata")
# 
# #Ocean salinity
# for (k in c("bot_sal","surf_sal")){
#   for (i in c("fall_spdf","spring_spdf")){
#     print(paste(k,i))
#     # suppressMessages(
#     assign(paste0(k,"_",i),
#            collapse_into_stack(folder_path = file.path(raw.dir, "salinity", k,i,"rasters")))
#     # )
#     save(list = paste0(k,"_",i), file = file.path(raw.dir,paste0(k,"_",i,".rdata")))
#   }
# }

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

# drive_ids <- dplyr::bind_rows(googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1_V3mHOc3M1DjLdjCrc7saW3-LvTiJPDd?usp=sharing"),
#                                     recursive = TRUE, n_max = Inf),
#                               googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1ZYJk7GtkGLTUOukCal8KIwl_rYXkYJWZ?usp=sharing"),
#                                                     recursive = TRUE, n_max = Inf)) %>% 
#   dplyr::select(-drive_resource) %>% 
#   data.frame

drive_ids<-dplyr::bind_rows(googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1fbz362catB6p-WmULpi3QTfKMV9YBn6y"), 
                      recursive = TRUE, n_max = Inf)) %>% 
  dplyr::select(-drive_resource) %>% 
  data.frame

drive_list <- drive_ids %>%
  dplyr::filter(stringr::str_detect(name, pattern = "RAST_(?=.*PA)")) %>%
  tidyr::separate(col = name, into = c("stock_name", "A", "B", "C", "season", "D", "E", "F"), sep = "\\.", remove = FALSE) %>% 
  tidyr::separate(col = stock_name, into = c("G", "species_code", "year"), sep = "_") %>% 
  dplyr::select(-A, -B, -C, -D, -E, -F, -G) %>% 
  dplyr::arrange(species_code, season, year) %>% 
  dplyr::distinct(.keep_all = TRUE)

drive_list %>% 
  #dplyr::filter(species_code == "winflo") %>% 
  dplyr::filter(!species_code %in% c("blabas", "bluefi", "monkfh", "scupzz")) %>%
  dplyr::mutate(path = paste0(path.expand(folder_path), "\\", name)) %>% 
  purrr::pmap(function(id, path, ...) googledrive::drive_download(
    file = googledrive::as_id(id),
    path = path,
    overwrite = TRUE))


drive_list %>% 
   #dplyr::filter(species_code == "acared") %>% 
  dplyr::filter(!species_code %in% c("blabas", "bluefi", "monkfh", "scupzz")) %>%
  select(species_code, season) %>% 
  distinct(.keep_all = TRUE) %>% 
  purrr::pmap(function(season, species_code, ...){
    x = collapse_into_stack(folder_path, fname_regex = sprintf("(?=.*%s)(?=.*%s)", species_code, season))
    fname = sprintf("data-raw/%s_%s_occ_prob", season, species_code)
    assign(fname, x)
    save(list = fname, file = sprintf("%s.rdata", fname))
  }
  )



fall_atlher_occ_prob <- collapse_into_stack(tmp_dir,
                                            fname_regex = sprintf("(?=.*atlmac)(?=.*%s)", "fall"))


