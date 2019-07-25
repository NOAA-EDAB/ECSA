render_ecsa <- function(input_file){
  
  #Extract Rmd from ECSA project directory
  prev_wd <- here::here()
  rmd <- readr::read_lines(file.path(prev_wd,paste0("docs/",input_file)))
  output <- readr::read_lines(file.path(prev_wd,"docs/_output.yml"))
  
  #Create a temporary directory to build the book
  wd <- tempdir()
  
  #bookdown::render_book() requires that your input_file is in the wd
  #NOTE: Bookdown will try to knit all Rmds into one (and break) if you try to knit 
  #directly from /docs
  setwd(wd)
  file.remove(file.path(list.files(wd))) 
  
  #Pull out name of file without extension
  stock <- str_remove(input_file, "\\.rmd|\\.Rmd")
  
  #Creates yml file in temp directory. Important for getting download button
  temp_book_yml <- paste0('book_filename: "',paste0(stock),'_.rmd"\ndelete_merged_file: true')
  
  #Write critical files for creating book with downloadable pdfs
  readr::write_lines(temp_book_yml, file.path(wd, "_bookdown.yml"))
  write(rmd, file.path(wd,input_file))
  write(output, file.path(wd,"_output.yml"))
  bookdown::render_book(input = input_file,
                        output_format = "all",
                        output_dir = file.path(prev_wd,"docs"))
  
  #Simplifies filenames by removing extra underscores. These underscores are necessary to build
  #the pdf
  file.rename(from = file.path(prev_wd,"docs",paste0(stock,"_.pdf")),
              to = file.path(prev_wd,"docs",paste0(stock,".pdf")))
  
  file.rename(from = file.path(prev_wd,"docs",paste0(stock,"_.html")),
              to = file.path(prev_wd,"docs",paste0(stock,".html")))
  
  #Read in html and alter pdf filename for downloading 
  html <- readr::read_lines(file.path(prev_wd, "docs",paste0(stock,".html")))
  html <- gsub(paste0(stock,'_\\.pdf'),
               paste0(stock,'\\.pdf'),
               html)
  html <- readr::write_lines(html, file.path(prev_wd, "docs",paste0(stock,".html")))
  
  #Get back to the original directory
  setwd(prev_wd)
  
  message(paste(input_file, "rendered to"), prev_wd)
  
}
