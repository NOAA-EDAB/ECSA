tab_plotly <- function(df, showlegend = T){
  
  if (showlegend) {
    p <- plot_ly(type = 'scatter', mode = 'lines', showlegend = T)
  } else {
    p <- plot_ly(type = 'scatter', mode = 'lines', showlegend = F)
  }
  
  plotvars <- names(df)[2:ncol(df)]
  for (i in 1:length(plotvars)){
    plot_df <- df %>%
      rename(data=one_of(plotvars[i])) %>%
      dplyr::select(Time, data)
    
    p <- p %>%
      add_lines(data=plot_df, x=~Time, y=~data, name=plotvars[i]) 
  }
  

  
  return(p)
}

