create_template <- function(survdat_name = "SUMMER FLOUNDER",
                            overwrite = FALSE){
    
  library(dplyr)
  
  load("data/df_survdat.rda")
  spp <- read.csv("data/species_list.csv")
  svspp <- unique(as.numeric(spp[spp$com_name == survdat_name,]$svspp))
  
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  clean_names <- df_survdat %>%
    filter(COMNAME == survdat_name) %>% 
    dplyr::select(SCINAME, COMNAME) %>% 
    distinct() %>% 
    mutate(common_name = tolower(COMNAME),
           sci_name = firstup(tolower(SCINAME)),
           cc_name = gsub(" ", "-", common_name)) %>% 
    tidyr::extract(common_name, into = c('partA', 'partB'), '(.*)\\s+([^ ]+)$', remove = FALSE) %>% 
    mutate(partA = substr(partA, start = 1, stop = 3),
           partB = substr(partB, start = 1, stop = 3),
           partC = ifelse(is.na(partA) | is.na(partB),
                          substr(common_name, start = 1, stop = 6),
                          paste0(partA, partB)),
           species_code = stringr::str_pad(partC, 6, pad = "z", side = "right")) %>% 
    dplyr::select(-partA,
                  -partB,
                  -partC,
                  -SCINAME,
                  -COMNAME)
  
  dat <- readLines("generic_template.rmd")
  dat <- gsub("\\{\\{COMMON_NAME\\}\\}", clean_names$common_name, dat)
  dat <- gsub("\\{\\{SCI_NAME\\}\\}", clean_names$sci_name, dat)
  dat <- gsub("\\{\\{CC_NAME\\}\\}", clean_names$cc_name, dat)
  dat <- gsub("\\{\\{SPECIES_CODE\\}\\}", clean_names$species_code, dat)  
  dat <- gsub("\\{\\{svspp\\}\\}", svspp, dat)  
  file_name <- sprintf("ECSA_%s.rmd", clean_names$cc_name)

  if(file.exists(file_name) &
     !overwrite){
    stop(sprintf("\nEasy, Cowboy!\n%s already exists. If you want to do this, change 'overwrite = TRUE'",
                 file_name))
  }
  
  file_connection <- file(file_name)
  writeLines(dat, file_connection)
  close(file_connection)
}
