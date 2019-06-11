#' Create a ECSA template
#'
#' A function to create an ECSA bookdown template for a species of interest. 
#'
#'
#' @param survdat_name A common species name as defined by survdat. Options as of 8/24 include 
#' JONAH CRAB, AMERICAN LOBSTER, and SUMMER FLOUNDER.
#' @param output_dir The output directory for the compiled bookdown HTML document and supporting files.
#' @param book_filename The filename of the compiled bookdown HTML document. Defaults "to ECSA_\code{survdat_name}.html".
#' @param render_book Logical. If TRUE, the generated template will render a bookdown HTML document into the chosen directory.
#' Otherwise, a template .Rmd file will be generated in the ECSA package directory.   
#' @param overwrite Logical. If TRUE, output will overwrite any existing template for chosen species.
#' @param make_interactive Logical. If TRUE, template time series will be made interative through the plotly library.
#'
#' @return A .Rmd file populated with figures that can be knit into an ECSA report skeleton. 
#'
#' @export
#' 
#' @examples  
#'
#' create_template(survdat_name = "SMOOTH DOGFISH", overwrite = T, output_dir = getwd())
#'


create_template <- function(survdat_name,
                            stock_code,
                            sci_name,
                            overwrite = FALSE,
                            make_interactive = FALSE,
                            output_dir = NULL,
                            book_filename = NULL,
                            render_book = F){
  
  # load("data/df_survdat.rda")
  # spp <- read.csv("data/species_list.csv")
  # svspp <- unique(as.numeric(spp[spp$com_name == survdat_name,]$svspp))
  
  # firstup <- function(x) {
  #   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  #   x
  # }
  
  `%>%` <- magrittr::`%>%`
  
  clean_names <- read.csv("data/stock_data/stock_list.csv") %>% 
    dplyr::select(sp) %>% 
    dplyr::filter(sp == tolower(stock_code)) %>% 
    dplyr::distinct(.keep_all = TRUE) %>% 
    dplyr::mutate(cc_name = survdat_name,
                  common_name = stringr::str_to_title(stringr::str_replace(survdat_name, "-", " ")),
                  sci_name = sci_name)
  
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
  dat <- gsub("\\{\\{SPECIES_CODE\\}\\}", clean_names$sp, dat)  
  # dat <- gsub("\\{\\{SVSPP\\}\\}", clean_names$svspp, dat)  
  # dat <- gsub("\\{\\{INTERACTIVE\\}\\}", make_interactive, dat)
  file_name <- sprintf("ECSA_%s.rmd", clean_names$cc_name)

  #Adjust _bookdown.yml accordingly
  bookyml <- suppressWarnings(
    readLines("templates/_bookdown_template.yml")
  )
  bookyml <- stringr::str_replace(bookyml,
                                  "\\[.*\\]",
                                  sprintf('["ECSA_%s.rmd"]',
                                          clean_names$cc_name))
  
  #Set output directory for bookdown files
  if(is.null(output_dir)){
    stop("Output directory must be specified.")
  } else {
    bookyml <- stringr::str_replace(bookyml,
                                    'output_dir: ".*"',
                                    sprintf('output_dir: "%s"',
                                    output_dir))
  }
  
  #Set filename of final HTML document (by default this is the title of the template)
  if (!is.null(book_filename)) {
    bookyml <- stringr::str_replace(bookyml,
                                    'book_filename: ".*"',
                                    sprintf('book_filename: "%s"',
                                            book_filename))
  } else {
    bookyml <- stringr::str_replace(bookyml,
                                    'book_filename: ".*"',
                                    sprintf('book_filename: "ECSA_%s"',
                                            clean_names$cc_name))
  }
  
  #Check to make sure existing file is not over-written
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
  
  book_connection <- file("_bookdown.yml",open = "w")
  writeLines(bookyml, book_connection)
  close(book_connection)
  message("\n_bookdown.yml successfully created.")
  
  if (render_book){
    bookdown::render_book(sprintf("ECSA_%s.rmd",
                                  clean_names$cc_name))
  }

}
# create_template(survdat_name = "ATLANTIC-MENHADEN",
#                 stock_code = "atlmen",
#                 sci_name  = "Brevoortia tyrannus",
#                   output_dir = "~/bookdown_output", #Creates new folder by default
#                 overwrite = T)
# bookdown::render_book("ECSA_ATLANTIC-MENHADEN.rmd")
