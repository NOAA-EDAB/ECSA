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
                            send_to_google_doc = FALSE,
                            overwrite = FALSE) {
  
  `%>%` <- magrittr::`%>%`
  library(readr)

  ## Select the stock and format stock area and common name
  clean_names <- readr::read_csv(here::here("data/stock_data/stock_list.csv"),
                                 col_types = readr::cols(
                                   common_name = readr::col_character(),
                                   sci_name = readr::col_character(),
                                   cc_name = readr::col_character(),
                                   stock_name = readr::col_character(),
                                   species_code = readr::col_character(),
                                   svspp = readr::col_double(),
                                   stock_season = readr::col_character(),
                                   strata = readr::col_double())) %>% 
    dplyr::mutate(stock_subarea = ifelse(is.na(stock_subarea), "",
                                         sprintf("%s ", stock_subarea)),
                  pattern = sprintf("^%s$", !!stock_name)) %>% 
    dplyr::filter(stringr::str_detect(stock_name, pattern)) 
  
  if (nrow(clean_names) > 1){
    #This splits out a stock area from the stock name. If there's no stock area associated with the name, then
    #this step returns a warning (hence the use of suppressWarnings())
     suppressWarnings( clean_names %<>% 
        tidyr::separate(.,stock_name, c("name","area"), "_", remove = FALSE) %>% 
        dplyr::slice(1) )
  } else if (nrow(clean_names) < 1){
    stop(sprintf("'%s' is not found. Check spelling or add '%s' as a new stock to '%s'", stock_name, stock_name, path.expand("data/stock_data/stock_list.csv")))
  }

  #Create .Rmd file to be written to book
  dat <- readLines(here::here("templates","generic_template1.rmd"))
  dat <- gsub("\\{\\{COMMON_NAME\\}\\}", clean_names$common_name, dat)
  dat <- gsub("\\{\\{STOCK_NAME\\}\\}", clean_names$stock_name, dat)
  dat <- gsub("\\{\\{STOCK_SUBAREA\\}\\}", clean_names$stock_subarea, dat)
  dat <- gsub("\\{\\{SCI_NAME\\}\\}", clean_names$sci_name, dat)
  dat <- gsub("\\{\\{CC_NAME\\}\\}", clean_names$cc_name, dat)
  dat <- gsub("\\{\\{SPECIES_CODE\\}\\}", clean_names$svspp, dat)  

  # cat(dat,sep = "\n" )
  file_name <- sprintf("%s_draft.rmd", clean_names$stock_name)
  folder_name <- sprintf("%s",output_dir)
  #output_dir <- sprintf("%s_book", clean_names$stock_name)
  
  # create the output directory if missing
  if(!dir.exists(folder_name)) {
    dir.create(folder_name,recursive = TRUE)
  }

  #Check to make sure existing file is not over-written
  if(file.exists(sprintf("%s/%s",folder_name, file_name)) &  !overwrite){
    stop(sprintf("\nEasy, Cowboy!\n%s already exists. If you want to do this, change 'overwrite = TRUE'", file_name))
  } else {
    
    # writes generic template after species specific substitutions to .rmd
    file_connection <- file(sprintf("%s/%s", folder_name, file_name))
    writeLines(dat, file_connection)
    close(file_connection)
    
    message(sprintf("ECSA template written locally: %s\n",
                    sprintf("%s/%s", folder_name, file_name)))
    
  }
  
  if(send_to_google_doc) {
    gdoc_exist <- googledrive::drive_get(sprintf("EDABranch_Drive/Products/ECSA/%s", file_name))

    #Check to make sure existing file is not over-written
    if(nrow(gdoc_exist) > 0 &
       !overwrite) {
      stop(sprintf("\nEasy, Cowboy!\n%s already exists. If you want to do this, change 'overwrite = TRUE'", file_name))
    } else {
      message("Now to render to Google Drive...\n")
      
      markdrive::gdoc_render(filename = sprintf("%s/%s", folder_name, file_name),
                             gdoc_name = gsub(".rmd$", "", file_name),
                             gdoc_path = "EDABranch_Drive/Products/ECSA/")
      
      gdoc_link <- googledrive::drive_link(sprintf("EDABranch_Drive/Products/ECSA/%s", file_name))

      message(sprintf("ECSA template written as a google doc:%s\n",
                      gdoc_link))
    }
  }
}

