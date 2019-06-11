tab_plotly <- function(df, showlegend = T, series.name = NULL){
  
  if (showlegend) {
    p <- plot_ly(type = 'scatter', mode = 'lines', showlegend = T)
  } else {
    p <- plot_ly(type = 'scatter', mode = 'lines', showlegend = F)
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
    
    plot_df <- df %>%
      rename(data=one_of(plotvars[i])) %>%
      dplyr::select(Time, data)
    
    if (length(plotvars) <= 3) {
      p <- p %>%
        add_lines(data=plot_df, x=~Time, y=~data, name=plotvars[i], line = list(color = color)) 
    } else {
      p <- p %>%
        add_lines(data=plot_df, x=~Time, y=~data, name=plotvars[i]) 
    }
    
  }
  

  
  return(p)
}

