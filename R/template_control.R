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


create_template <- function(stock_name,
                            # stock_code,
                            # sci_name,
                            overwrite = FALSE,
                            make_interactive = FALSE,
                            # output_dir = "/docs",
                            book_filename = NULL,
                            render_book = F){
  
  `%>%` <- magrittr::`%>%`
  library(readr)
  
  ## Select the stock and format stock area and common name
  clean_names <- readr::read_csv("data/stock_data/stock_list.csv",
                                 col_types = readr::cols(
                                   common_name = col_character(),
                                   sci_name = col_character(),
                                   cc_name = col_character(),
                                   stock_name = col_character(),
                                   species_code = col_character(),
                                   svspp = col_double(),
                                   season = col_character(),
                                   strata = col_double()
                                 )) %>%
    dplyr::filter(stock_name == !!stock_name) %>%
    dplyr::mutate(stock_area = dplyr::case_when(grepl("_sne-ma$", stock_name) ~ "southern New England/mid-Atlantic ",
                                                 grepl("_cc$", stock_name) ~ "Cape Cod ",
                                                grepl("_sne$", stock_name) ~ "southern New England ",
                                                grepl("_gom-ngb$", stock_name) ~ "Gulf of Maine/northern Georges Bank ",
                                                grepl("_sgb-ma$", stock_name) ~ "southern Georges Bank/mid-Atlantic ",
                                                grepl("_gb$", stock_name) ~ "Georges Bank ",
                                                grepl("_gom-ngb$", stock_name) ~ "Gulf of Maine/northern Georges Bank ",
                                                grepl("_gom$", stock_name) ~ "Gulf of Maine ",
                                                grepl("_gom-gb$", stock_name) ~ "Gulf of Maine/Georges Bank ",
                                                grepl("_north$", stock_name) ~ "north ",
                                                grepl("_south$", stock_name) ~ "south ",
                                                 TRUE ~ ""),
                  common_name = gsub("^atlantic", "Atlantic", common_name),
                  common_name = gsub("^american", "American", common_name),
                  common_name = gsub("^acadian", "Acadian", common_name)) %>% 
    dplyr::select(common_name,
           stock_name,
           stock_area,
           sci_name,
           cc_name,
           svspp) %>% 
    dplyr::distinct(.keep_all = TRUE)
    
  if(length(clean_names) < 1){
    stop(sprintf("%s is not found. Check spelling or add %s as a new stock to '%s'", stock_name, stock_name, path.expand("data/stock_data/stock_list.csv")))
  }

  #Create .Rmd file to be written to book
  dat <- readLines("templates/generic_template.rmd")
  dat <- gsub("\\{\\{COMMON_NAME\\}\\}", clean_names$common_name, dat)
  dat <- gsub("\\{\\{STOCK_AREA\\}\\}", clean_names$stock_area, dat)
  dat <- gsub("\\{\\{SCI_NAME\\}\\}", clean_names$sci_name, dat)
  dat <- gsub("\\{\\{CC_NAME\\}\\}", clean_names$cc_name, dat)
  dat <- gsub("\\{\\{SPECIES_CODE\\}\\}", clean_names$svspp, dat)  

  # cat(dat,sep = "\n" )
  file_name <- sprintf("ECSA_%s.rmd", clean_names$stock_name)
  folder_name <- sprintf("docs/%s", clean_names$stock_name)

  if(!dir.exists(folder_name)) {
    dir.create(folder_name)
  }

  #Adjust _bookdown.yml accordingly
  bookyml <- suppressWarnings(
    readLines("templates/_bookdown_template.yml")
  )
  bookyml <- stringr::str_replace(bookyml,
                                  "\\[.*\\]",
                                  sprintf('["ECSA_%s.rmd"]',
                                          clean_names$stock_name))
  # cat(bookyml, sep = "\n")
  
  #Set output directory for bookdown files
  # if(!is.null(output_dir)){
  #   bookyml <- stringr::str_replace(bookyml,
  #                                   'output_dir: ".*"',
  #                                   sprintf('output_dir: "%s"',
  #                                           output_dir))
  # } else {
  bookyml <- stringr::str_replace(bookyml,
                                  'output_dir: ".*"',
                                  sprintf('output_dir: "%s"',
                                          folder_name))
  # }
  
  #Set filename of final HTML document (by default this is the title of the template)
  # if (!is.null(book_filename)) {
    # bookyml <- stringr::str_replace(bookyml,
                                    # 'book_filename: ".*"',
                                    # sprintf('book_filename: "%s"',
                                            # book_filename))
  # } else {
    bookyml <- stringr::str_replace(bookyml,
                                    'book_filename: ".*"',
                                    sprintf('book_filename: "ECSA_%s"',
                                            clean_names$stock_name))
  # }
  
  #Check to make sure existing file is not over-written
  if(file.exists(file_name) &
     !overwrite){
    stop(sprintf("\nEasy, Cowboy!\n%s already exists. If you want to do this, change 'overwrite = TRUE'",
                 file_name))
  }
  
  file_connection <- file(sprintf("%s/%s", 
                                  folder_name, 
                                  file_name))
  writeLines(dat, file_connection)
  close(file_connection)
  message(sprintf("ECSA template written to %s",
                  sprintf("%s/%s", folder_name, file_name)))
  
  book_connection <- file(sprintf("%s/_bookdown.yml",
                                  folder_name),
                          open = "w")
  writeLines(bookyml, book_connection)
  close(book_connection)
  message(sprintf("\n%s/_bookdown.yml successfully created.",
                  folder_name))
  
  if (render_book){
    bookdown::render_book(sprintf("%s/%s",
                                  folder_name,
                                  file_name))
  }
  if(file.exists(sprintf("%s/ECSA_%s.html", 
                         folder_name,
                         clean_names$stock_name))) {
    
  }
  
}
# create_template(survdat_name = "ATLANTIC-MENHADEN",
#                 stock_code = "atlmen",
#                 sci_name  = "Brevoortia tyrannus",
#                   output_dir = "~/bookdown_output", #Creates new folder by default
#                 overwrite = T)
# bookdown::render_book("ECSA_ATLANTIC-MENHADEN.rmd")
