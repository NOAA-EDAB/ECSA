#' Create a ECSA template
#'
#' A function to create an ECSA Rmarkdown template given the common name of a species of interest. 
#'
#'
#' @param survdat_name A common species name as defined by survdat. Options as of 8/24 include 
#' JONAH CRAB, AMERICAN LOBSTER, and SUMMER FLOUNDER.
#' @param overwrite Logical. If TRUE, output will overwrite any existing template for chosen species.
#' @param make_interactive Logical. If TRUE, template time series will be made interative through the ggiraph library.
#'
#' @return A .Rmd file populated with figures that can be knit into an ECSA report skeleton. 
#'
#' @export
#' @examples  
#'
#' create_template(survdat_name = "SUMMER FLOUNDER", overwrite = TRUE)
#'


create_template <- function(survdat_name,
                            overwrite = FALSE,
                            make_interactive = FALSE){
  
  load("data/df_survdat.rda")
  spp <- read.csv("data/species_list.csv")
  svspp <- unique(as.numeric(spp[spp$com_name == survdat_name,]$svspp))
  
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  clean_names <- df_survdat %>%
    dplyr::filter(COMNAME == survdat_name) %>% 
    dplyr::select(SCINAME, COMNAME) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(common_name = tolower(COMNAME),
           sci_name = firstup(tolower(SCINAME)),
           cc_name = gsub(" ", "-", common_name)) %>% 
    tidyr::extract(common_name, into = c('partA', 'partB'), '(.*)\\s+([^ ]+)$', remove = FALSE) %>% 
    dplyr::mutate(partA = substr(partA, start = 1, stop = 3),
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
  dat <- gsub("\\{\\{SVSPP\\}\\}", svspp, dat)  
  dat <- gsub("\\{\\{INTERACTIVE\\}\\}", make_interactive, dat)
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
