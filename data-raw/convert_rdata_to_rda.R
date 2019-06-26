for (i in list.files("data-raw")){
  
  if (stringr::str_detect(i,"rdata")){
    
    var <- loadRData(file.path("data-raw",i))
    fname <- stringr::str_remove(i, ".rdata")
    
    save(var,file = file.path("data-raw",paste0(fname,".rda")))
  }
}

