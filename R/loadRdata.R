#' An internal function to load and assign .Rdata files custom names.
#' Taken from this SO comment: https://stackoverflow.com/a/25455968/9908798
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}