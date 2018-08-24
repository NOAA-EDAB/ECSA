#' Download strata from nefsc ftp
#'
#' @param overwrite 
#'
#' @return NULL
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' get_strata(overwrite = FALSE)
#' }
#' 
get_strata <- function(overwrite = FALSE) {
  
  url <- "ftp://ftp.nefsc.noaa.gov/pub/gis/"
  
  all_dir <- RCurl::getURL(url, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE)
  all_dir <- paste(url, strsplit(all_dir, "\r*\n")[[1]], sep = "")
  
  strata_dir <- grep("BTS_Strata", all_dir, value = TRUE)
  for(i in 1:length(strata_dir)){
    file <- sprintf("data/strata_shapefiles/%s", basename(strata_dir[i]))
    if(!file.exists(file) |
       overwrite == TRUE){
      download.file(strata_dir[i], file, method = "auto", mode = "wb")
    }
  }
}
