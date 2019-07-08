#A function to write ERDDAP datasets.xml for .nc files


write_erddapXML <- function(variable_name,
                            variable_short_name,
                            date_range,
                            geographic_range,
                            version,
                            data_type,
                            output_dir,
                            keywords,
                            summary,
                            infoUrl){


  datasetID <- sprintf("%s_%s_%s",
                       variable_name,
                       data_type,
                       version)
  fileNameRegex <- sprintf("%s, %s, %s, %s.nc",
                           variable_name,
                           date_range,
                           geographic_range,
                           version)
  xml_title <- stringr::str_remove(fileNameRegex, "\\.nc")

  #Read xml
  template_xml <- xml2::read_xml(here::here("templates/erddap_xml_template.xml"))
  
  
  #Change datasetID
  xml2::xml_attr(template_xml, "datasetID") <- datasetID
  
  #Convert to list
  template_xml_list <- xml2::as_list(template_xml)
  
  #Change fileNameRegex
  template_xml_list[[1]]$fileNameRegex[[1]] <- fileNameRegex
  
  #Set data set reload time. This is necessary for valid xml output but should remain unchanged
  template_xml_list[[1]]$reloadEveryNMinutes[[1]] <- "10080"
  
  #Change data set attributes
  for (i in 1:length(template_xml_list[[1]]$addAttributes)){
    
    att <- template_xml_list[[1]]$addAttributes[[i]]
    
    if ( attr(att, "name") == "infoUrl" ) {
      template_xml_list[[1]]$addAttributes[[i]][1] <- infoUrl
      
    } else if ( attr(att, "name") == "keywords" ) {
      template_xml_list[[1]]$addAttributes[[i]][1] <- keywords
      
    } else if ( attr(att, "name") == "title" ) {
      template_xml_list[[1]]$addAttributes[[i]][1] <- xml_title
      
    } else if ( attr(att, "name") == "summary" ) {
      template_xml_list[[1]]$addAttributes[[i]][1] <- summary
      
    }   
  }
  
  template_xml_list[[1]]$dataVariable$destinationName[[1]] <- variable_short_name
  
  #remove [ comment ] lines
  template_xml_list[[1]][[9]][[3]] <- template_xml_list[[1]][[9]][[2]][[1]] 
  template_xml_list[[1]][[10]][[3]] <- template_xml_list[[1]][[10]][[2]][[1]] 
  template_xml_list[[1]][[11]][[4]] <- variable_short_name
  
  #Convert back to xml
  out <- xml2::as_xml_document(template_xml_list)
  
  #Write out as xml
  xml2::write_xml(out, file = paste0(output_dir,"/", xml_title,".xml"))

}

#Example
# write_erddapXML(variable_name = "fall bottom temp",
#                 variable_short_name = "bottom temp",
#                 date_range = "1991-2019",
#                 geographic_range = "NE LME",
#                 version = "2",
#                 data_type = "gridded",
#                 output_dir = here::here(),
#                 infoUrl = "www.github.com",
#                 keywords = "keyword",
#                 summary = "a data set")

