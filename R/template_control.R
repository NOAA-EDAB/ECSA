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
#' create_template(survdat_name = "SUMMER FLOUNDER", overwrite = TRUE, make_interactive = FALSE)
#'


create_template <- function(survdat_name,
                            overwrite = FALSE,
                            make_interactive = FALSE){
  
  # load("data/df_survdat.rda")
  # spp <- read.csv("data/species_list.csv")
  # svspp <- unique(as.numeric(spp[spp$com_name == survdat_name,]$svspp))
  
  # firstup <- function(x) {
  #   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  #   x
  # }
  
  `%>%` <- magrittr::`%>%`
  
  clean_names <- read.csv("data/stock_list.csv") %>% 
    dplyr::select(common_name,
                  sci_name,
                  cc_name,
                  species_code,
                  svspp) %>% 
    dplyr::filter(common_name == tolower(survdat_name)) %>% 
    dplyr::distinct(.keep_all = TRUE)
  
  if(length(clean_names) < 1){
    stop(sprintf("%s is not found. Check spelling or add %s as a new stock to '%s'", survdat_name, survdat_name, path.expand("data/stock_list.csv")))
  }
  # 
  # clean_names <- df_survdat %>%
  #   dplyr::filter(COMNAME == survdat_name) %>% 
  #   dplyr::select(SCINAME, COMNAME) %>% 
  #   dplyr::distinct() %>% 
  #   dplyr::mutate(common_name = tolower(COMNAME),
  #          sci_name = firstup(tolower(SCINAME)),
  #          cc_name = gsub(" ", "-", common_name)) %>% 
  #   tidyr::extract(common_name, into = c('partA', 'partB'), '(.*)\\s+([^ ]+)$', remove = FALSE) %>% 
  #   dplyr::mutate(partA = substr(partA, start = 1, stop = 3),
  #          partB = substr(partB, start = 1, stop = 3),
  #          partC = ifelse(is.na(partA) | is.na(partB),
  #                         substr(common_name, start = 1, stop = 6),
  #                         paste0(partA, partB)),
  #          species_code = stringr::str_pad(partC, 6, pad = "z", side = "right")) %>% 
  #   dplyr::select(-partA,
  #                 -partB,
  #                 -partC,
  #                 -SCINAME,
  #                 -COMNAME)
  
  #Create .Rmd file to be written to book
  dat <- readLines("templates/generic_template.rmd")
  dat <- gsub("\\{\\{COMMON_NAME\\}\\}", clean_names$common_name, dat)
  dat <- gsub("\\{\\{SCI_NAME\\}\\}", clean_names$sci_name, dat)
  dat <- gsub("\\{\\{CC_NAME\\}\\}", clean_names$cc_name, dat)
  dat <- gsub("\\{\\{SPECIES_CODE\\}\\}", clean_names$species_code, dat)  
  dat <- gsub("\\{\\{SVSPP\\}\\}", clean_names$svspp, dat)  
  dat <- gsub("\\{\\{INTERACTIVE\\}\\}", make_interactive, dat)
  file_name <- sprintf("ECSA_%s.rmd", clean_names$cc_name)

  #Adjust _bookdown.yml accordingly
  bookyml <- readLines("_bookdown.yml")
  bookyml <- stringr::str_replace(bookyml,
                                  "\\[.*\\]",
                                  sprintf('["ECSA_%s.rmd"]',
                                          clean_names$cc_name))
  
  if(file.exists(file_name) &
     !overwrite){
    stop(sprintf("\nEasy, Cowboy!\n%s already exists. If you want to do this, change 'overwrite = TRUE'",
                 file_name))
  }
  
  file_connection <- file(file_name)
  writeLines(dat, file_connection)
  close(file_connection)
  message(sprintf("ECSA template written to %s",
                  file_name))
  
  book_connection <- file("_bookdown.yml")
  writeLines(bookyml, book_connection)
  close(book_connection)
  message("\n_bookdown.yml successfully updated.")

}
# create_template(survdat_name = "smooth dogfish", overwrite = T)
# bookdown::render_book("ECSA_smooth-dogfish.rmd")
