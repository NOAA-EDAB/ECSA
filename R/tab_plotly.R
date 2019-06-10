tab_plotly <- function(df, title, ylab){
  # updatemenus component
  updatemenus <- list(
    list(
      type = "buttons",
      direction = "right",
      xanchor = 'center',
      yanchor = "top",
      pad = list('r'= 0, 't'= 10, 'b' = 10),
      y = 1.17,
      buttons = list(
        list(
          label = "Fall",
          method = "update",
          args = list(list(visible = c(FALSE, TRUE)),
                      list(title = "Fall"))),
        list(
          label = "Spring",
          method = "update",
          args = list(list(visible = c(TRUE, FALSE)),
                      list(title = "Spring"))),
        list(
          label = "Both",
          method = "update",
          args = list(list(visible = c(TRUE, TRUE)),
                      list(title = "Spring and fall")))
      )
    )
  )
  p <- df %>%
    plot_ly(type = 'scatter', mode = 'lines') %>%
    add_lines(x=~Time, y=~Spring, name="Spring",
              line=list(color="#33CFA5")) %>%
    add_lines(x=~Time, y=~Fall, name="Fall",
              line=list(color="#F06A6A"))%>%
    layout(title = title, showlegend=FALSE,
           xaxis=list(title="Time"),
           yaxis=list(title=ylab,
                      size = 6),
           updatemenus=updatemenus)
  return(p)
}

