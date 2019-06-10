tab_plotly_zoo <- function(df, title, ylab, genera){
  
  
  annot <- list(list(text = "Select<br>genera:", x=0.15, y=-0.35,
                     xref='paper', yref='paper', showarrow=FALSE))
  # updatemenus component
  updatemenus <- list(
    list(
      type = "buttons",
      direction = "right",
      xanchor = 'center',
      yanchor = "top",
      pad = list('r'= 0, 't'= 10, 'b' = 10),
      x = 0.5,
      y = -0.2,
      buttons = list(
        list(
          label = genera[1],
          method = "update",
          args = list(list(visible = c(T, F, F, F, F)),
                      list(title = genera[1]))),
        list(
          label = genera[2],
          method = "update",
          args = list(list(visible = c(F, T, F, F, F)),
                      list(title = genera[2]))),
        list(
          label = genera[3],
          method = "update",
          args = list(list(visible = c(F, F, T, F, F)),
                      list(title = genera[3]))),
        list(
          label = genera[4],
          method = "update",
          args = list(list(visible = c(F, F, F, T, F)),
                      list(title = genera[4]))),
        list(
          label = genera[5],
          method = "update",
          args = list(list(visible = c(F, F, F, F, T)),
                      list(title = genera[5]))),
        list(
          label = "All",
          method = "update",
          args = list(list(visible = c(T, T, T, T, T)),
                      list(title = "All")))
      )
    )
  )
  
  
  p <- df %>%
    plot_ly(type = 'scatter', mode = 'lines') %>%
    add_lines(x=~Time, y=~get(genera[1]), name=genera[1]) %>%
    add_lines(x=~Time, y=~get(genera[2]), name=genera[2]) %>%
    add_lines(x=~Time, y=~get(genera[3]), name=genera[3]) %>%
    add_lines(x=~Time, y=~get(genera[4]), name=genera[4]) %>%
    add_lines(x=~Time, y=~get(genera[5]), name=genera[5]) %>%
    layout(title = title, showlegend=FALSE,
           xaxis=list(title="Time"),
           yaxis=list(title=ylab,
                      size = 6),
           updatemenus=updatemenus,
           annotations = annot)
  return(p)
}
