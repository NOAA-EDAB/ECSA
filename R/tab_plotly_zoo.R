tab_plotly_zoo <- function(df, title, ylab, genera, update_buttons){
  
  annot <- list(list(text = "Select<br>genera:", x=0.075, y=-0.375,
                     xref='paper', yref='paper', showarrow=FALSE))
  p <- df %>%
    plot_ly(type = 'scatter', mode = 'lines') %>%
    add_lines(x=~Time, y=~round(get(genera[1]),2), name=genera[1]) %>%
    add_lines(x=~Time, y=~round(get(genera[2]),2), name=genera[2]) %>%
    add_lines(x=~Time, y=~round(get(genera[3]),2), name=genera[3]) %>%
    add_lines(x=~Time, y=~round(get(genera[4]),2), name=genera[4]) %>%
    add_lines(x=~Time, y=~round(get(genera[5]),2), name=genera[5]) %>%
    layout(title = title, showlegend=FALSE,
           xaxis=list(title="Time"),
           yaxis=list(title=ylab,
                      size = 6),
           updatemenus=update_buttons,
           annotations = annot)
  return(p)
}