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


merge_to_bookdown <- function(file, 
                              output_dir = here::here("docs"),
                              render_book = T,
                              overwrite = F) {
  
  `%>%` <- magrittr::`%>%`
  library(readr)
  library(googledrive)
  
  
  #setwd(here::here())
  ## Select the stock and format stock area and common name
 
  ## Replace YAML
  # yaml::read_yaml("templates/_bookdown_template.yml")
  # drive_find("Hello world")
  stock_name <- "Hello world"
  tmp_txt <- tempfile(pattern = stock_name, fileext = ".txt")
  stock_file <- sprintf(here::here("docs/", stock_name, ".rmd"))
  
  googledrive::drive_download(
    sprintf("EDABranch_Drive/Products/ECSA/%s", stock_name),
    path = tmp_txt,
    overwrite = TRUE
  )
  
  docs_text <- readr::read_file(tmp_txt)
  
  intro_pattern <- ".*\\{\\{IntroductionStart\\}\\}(.*?)\\{\\{IntroductionEnd\\}\\}"
  intro_text <- regmatches(docs_text, regexec(pattern, docs_text))[[1]][2]


  stock_file <- here::here("docs", "atlantic-menhaden.rmd")

  #Create .Rmd file to be written to book
  rmd_text <- readr::read_file(stock_file)
  
  
  gsub("\\{\\{RMarkdownend\\}\\}", "sassy", dat)
  gsub(".*\\{\\{IntroductionStart\\}\\}(.*?)\\{\\{IntroductionEnd\\}\\}", "\\1", docs_text)
  
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

