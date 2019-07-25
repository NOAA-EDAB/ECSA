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


merge_to_bookdown <- function(stock_name, 
                              output_dir = here::here("docs"),
                              render_book = TRUE,
                              methods_gdoc_path = "generic_methods",
                              overwrite = FALSE) {
  
  `%>%` <- magrittr::`%>%`
  library(readr)
  library(googledrive)

  ### Helper functions

  ## Function to mustache the section names
  grab_text <- function(object, name) {
    pattern <- sprintf(".*\\{\\{%sStart\\}\\}(.*?)\\{\\{%sEnd\\}\\}", name, name)
    regmatches(object, regexec(pattern, object))[[1]][2]
  }
    

  # stock_name <- "scup"
  # stock_file <- sprintf("%s.rmd", here::here("docs", stock_name))
  
  ### Load the necessary files
  ## Download the edited google doc 
  tmp_txt <- tempfile(pattern = stock_name, fileext = ".txt")
  googledrive::drive_download(
    sprintf("EDABranch_Drive/Products/ECSA/%s", stock_name),
    path = tmp_txt,
    overwrite = TRUE
  )
  # docs_text <- paste(readLines(tmp_txt, encoding = "UTF-8", warn = F), collapse = " ")
  docs_text <- readr::read_file(tmp_txt)
  ## remove readme
  docs_text <- gsub("\\{\\{READMEStart\\}\\}(.*?)\\{\\{READMEEnd\\}\\}\r\n", "", docs_text)
  ## Fix the methods links
  docs_text <- gsub("#methods(.*?)\\)", "[methods](#methods\\1))", docs_text)
  
  ## Download the methods google doc  
  # methods_txt <- tempfile(pattern = stock_name, fileext = ".txt")
  # googledrive::drive_download(
  #   sprintf("EDABranch_Drive/Products/ECSA/%s", methods_gdoc_path),
  #   path = methods_txt,
  #   overwrite = TRUE
  # )
  # methods_text <- readr::read_file(methods_txt)
  
  ## Download the draft rmd
  rmd_text <- readr::read_file(sprintf("%s_draft.rmd", here::here("docs", stock_name)))
  ## remove readme
  rmd_text <- gsub("\\{\\{READMEStart\\}\\}(.*?)\\{\\{READMEEnd\\}\\}\n\n", "", rmd_text)

  ## Get the stock name, common name, and subarea
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
    dplyr::filter(stock_name == !!stock_name) %>%
    dplyr::select(stock_name, common_name, stock_subarea) %>% 
    dplyr::distinct(.keep_all = TRUE) %>% 
    dplyr::mutate(stock_subarea = ifelse(is.na(stock_subarea), "",
                                         sprintf("%s ", stock_subarea)))
  
  ## Get the new YAML
  yml <- yaml::read_yaml(here::here("templates/_bookdown_template.yml"))
  yml$title <- gsub("\\{\\{COMMON_NAME\\}\\}", clean_names$common_name, yml$title)
  yml$title <- gsub("\\{\\{STOCK_SUBAREA\\}\\}", clean_names$stock_subarea, yml$title)
  yml$bibliography <- here::here("docs/ECSA_bibliography.bib")
  
  ### Replace the text
  
  ## Extract the section names from the edited google doc
  doc_names <- stringr::str_extract_all(docs_text, "\\{\\{(.*)Start\\}\\}")[[1]]
  doc_names <- gsub("\\{\\{(.*?)Start\\}\\}", "\\1", doc_names)
  
  ## Extract the text from the edited google doc
  text_list <- lapply(doc_names, grab_text, object = docs_text)
  names(text_list) <- doc_names
  
  ## Pattern used to find and replace sections
  pattern <- sprintf("\\{\\{%sStart\\}\\}(.*?)\\{\\{%sEnd\\}\\}", names(text_list), names(text_list))

  ## 
  new_text <- rmd_text
  for(i in 1:length(text_list)) {
    new_text <- gsub(pattern[i], text_list[[i]], new_text)
  }
  
  new_text <- str_replace(new_text, " \\x{030A}| \\x{00B0}",
                           "`r degree`")
  
  new_text <- str_replace(new_text, "oC",
                           "`r paste0(degree,'C')`")

 ## Remove extra brackets
 new_text <- gsub("\n\\{\\{.*\\}\\}\n", "", new_text)

 new_text <- gsub("---(.*?)---",
  sprintf("---\n%s---", yaml::as.yaml(yml)), new_text)
  
  ##Create .Rmd file to be written to book
  file_name <- sprintf("%s.rmd", stock_name)
  folder_name <- sprintf("%s",output_dir)

  # create the output directory if missing
  if(!dir.exists(folder_name)) {
    dir.create(folder_name,recursive = T)
  }
   
  
  #Check to make sure existing file is not over-written
  if(file.exists(sprintf("%s/%s",folder_name,file_name)) &  !overwrite){
    stop(sprintf("\nEasy, Cowboy!\n%s already exists. If you want to do this, change 'overwrite = TRUE'", file_name))
  }
  

  
  # writes generic template after species specific substitutions to .rmd
  file_connection <- file(sprintf("%s/%s", folder_name, file_name))
  writeLines(new_text, file_connection, sep = "")
  
  methods <- readr::read_lines(here::here("templates/generic_methods.Rmd"), skip = 10)
  readr::write_lines(methods, file_connection, append = TRUE)
  # close(file_connection)
  
  message(sprintf("ECSA template written to %s",
                  sprintf("%s/%s", folder_name, file_name)))
  
}

