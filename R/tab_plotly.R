#' What
#' 
#' Describe
#' 
#' @param df
#' @param showlegend
#' @param series.name
#' 
#' @return 
#'
#' @importFrom magrittr "%>%"
#'

tab_plotly <- function(df, showlegend = T, series.name = NULL, add_smoother = F){
  
  `%>%` <- magrittr::`%>%`
  
  if (showlegend) {
    p <- plotly::plot_ly(type = 'scatter', mode = 'lines', showlegend = T)
  } else {
    p <- plotly::plot_ly(type = 'scatter', mode = 'lines', showlegend = F)
  }
  
  plotvars <- names(df)[2:ncol(df)]
  
  for (i in 1:length(plotvars)){
    
    if (plotvars[i] == "Regime.Mean" ) {
      color <- "#b2df8a"
    } else if (plotvars[i] == "Trend"| plotvars[i] == "Fall"){
      color <- "#a6cee3"
    } else if (plotvars[i] == "Series"| plotvars[i] == "Spring") {
      if (!is.null(series.name)){
        colnames(df)[colnames(df) == "Series"] <- series.name
        plotvars[i] <- series.name
        }
      color <- "#1f78b4"
    } 
    
    if (!add_smoother){
      
      plot_df <- df %>%
        dplyr::rename(data=one_of(plotvars[i])) %>%  ##make sure rename is from dplyr
        dplyr::select(Time, data)
      
    } else {
      
      smoothcol <- unlist(stringr::str_extract(plotvars,
                                               paste0(plotvars[i], "smooth")))
      smoothcol <- smoothcol[!is.na(smoothcol)]
      
      plot_df <- df %>%
        dplyr::select(Time,
                      data = which(colnames(.) == plotvars[i]),
                      smooth = which(str_detect(colnames(.), smoothcol)))
    }
    
    
    
    if (length(plotvars) <= 3) {
      p <- p %>%
        plotly::add_lines(data=plot_df, x=~Time, y=~data, name=plotvars[i],
                          line = list(color = color)) 
    } 
      
    if (add_smoother) {
        p <- p %>%
         plotly::add_trace(data=plot_df, x=~Time,
                            y=~data, 
                            name=plotvars[i], 
                            mode = 'markers',
                            showlegend = F) %>% 
          plotly::add_trace(data=plot_df,
                            x = ~Time,
                            y=~smooth,
                            mode = 'lines',
                            name=plotvars[i],
                            showlegend = F)
      }

      
    }
    
  
  return(p)
}
