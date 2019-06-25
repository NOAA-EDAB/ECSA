#'#' Create an ECSA bookdown template for a species of interest. 
#'
#'
#'
#' @param stock_name A common species name as defined by ??? (eg. atlantic-menhaden, smooth-dogfish)
#' @param output_dir The output directory for the compiled bookdown HTML document and supporting files. Default "docs" folder in current working directory
#' @param render_book Logical. If TRUE, the generated template will render a bookdown HTML document into the chosen directory and open in a web browser.
#' If FALSE, the template .Rmd file will be generated in the directory output_dir/stock-name folder   
#' @param overwrite Logical. If TRUE, output will overwrite any existing template for chosen species.
#'
#'
#'
#' @return A .Rmd file and/or bookdown html document populated with figures
#'
#' @importFrom magrittr "%>%"
#' 
#' @examples
#' 
#' create_template(stock_name = "smooth-dogfish", overwrite = T, output_dir = getwd(), render_book = T)
#' 
#' @export


create_template <- function(stock_name, 
                            output_dir = here::here("docs"),
                            render_book = T,
                            overwrite = F) {
  
  `%>%` <- magrittr::`%>%`
  library(readr)
  
  #setwd(here::here())
  ## Select the stock and format stock area and common name
  clean_names <- readr::read_csv(here::here("data","stock_data/stock_list.csv"),
                                 col_types = readr::cols(
                                   common_name = readr::col_character(),
                                   sci_name = readr::col_character(),
                                   cc_name = readr::col_character(),
                                   stock_name = readr::col_character(),
                                   species_code = readr::col_character(),
                                   svspp = readr::col_double(),
                                   stock_season = readr::col_character(),
                                   strata = readr::col_double()
                                 )) %>%
    dplyr::filter(stock_name == !!stock_name) %>%
    dplyr::mutate(stock_subarea = dplyr::case_when(grepl("_sne-ma$", stock_name) ~ "southern New England/mid-Atlantic ",
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
           stock_season,
           stock_subarea,
           sci_name,
           cc_name,
           svspp) %>% 
    dplyr::select(-stock_season) %>% 
    dplyr::distinct(.keep_all = TRUE)
    
  if(length(clean_names) < 1){
    stop(sprintf("%s is not found. Check spelling or add %s as a new stock to '%s'", stock_name, stock_name, path.expand("data/stock_data/stock_list.csv")))
  }

  #Create .Rmd file to be written to book
  dat <- readLines(here::here("templates","generic_template.rmd"))
  dat <- gsub("\\{\\{COMMON_NAME\\}\\}", clean_names$common_name, dat)
  dat <- gsub("\\{\\{STOCK_NAME\\}\\}", clean_names$stock_name, dat)
  dat <- gsub("\\{\\{STOCK_SUBAREA\\}\\}", clean_names$stock_subarea, dat)
  dat <- gsub("\\{\\{SCI_NAME\\}\\}", clean_names$sci_name, dat)
  dat <- gsub("\\{\\{CC_NAME\\}\\}", clean_names$cc_name, dat)
  dat <- gsub("\\{\\{SPECIES_CODE\\}\\}", clean_names$svspp, dat)  

  # cat(dat,sep = "\n" )
  file_name <- sprintf("%s.rmd", clean_names$stock_name)
  folder_name <- sprintf("%s",output_dir)
  #output_dir <- sprintf("%s_book", clean_names$stock_name)
  
  # create the output directory if missing
  if(!dir.exists(folder_name)) {
    dir.create(folder_name,recursive = T)
  }
# 
#   #Adjust _bookdown.yml accordingly
#   bookyml <- suppressWarnings(
#     readLines(here::here("templates","_bookdown_template.yml"))
#   )
#   # replace .rmd file name
#   bookyml <- stringr::str_replace(bookyml, "\\[.*\\]", sprintf('["ECSA_%s.rmd"]', clean_names$stock_name))
#   #Set output directory for bookdown files
#   bookyml <- stringr::str_replace(bookyml, 'output_dir: .*', sprintf('output_dir: %s', output_dir))
#   #Set filename of final HTML document (by default this is the title of the template)
#   bookyml <- stringr::str_replace(bookyml, 'book_filename: ".*"', 
#                                 sprintf('book_filename: "ECSA_%s_working_draft"', clean_names$stock_name))

  #Check to make sure existing file is not over-written
  if(file.exists(sprintf("%s/%s",folder_name,file_name)) &  !overwrite){
    stop(sprintf("\nEasy, Cowboy!\n%s already exists. If you want to do this, change 'overwrite = TRUE'", file_name))
  }
  
  # writes generic template after species specific substitutions to .rmd
  file_connection <- file(sprintf("%s/%s", folder_name, file_name))
  writeLines(dat, file_connection)
  close(file_connection)
  
  message(sprintf("ECSA template written to %s",
                  sprintf("%s/%s", folder_name, file_name)))
  
  # copy _bookdown.yml to differnt location
  # book_connection <- file(sprintf("%s/_bookdown.yml", folder_name), open = "w")
  # writeLines(bookyml, book_connection)
  # close(book_connection)
  # 
  # message(sprintf("\n%s/_bookdown.yml successfully created.", folder_name))


  # render the species specific markdown file into book
  if (render_book){
    pathToDir <- sprintf("%s", folder_name)
    pathToRmd <- sprintf("%s/%s", folder_name,file_name)
    #bookdown::render_book(sprintf("%s/%s",folder_name,file_name))
    rmarkdown::render(pathToRmd)
    browseURL(paste0(pathToDir,"/overview.html"))
  }
  

}

