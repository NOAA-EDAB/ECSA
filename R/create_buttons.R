#' Add column names to a plotly figure as buttons by writing them into the correct list structure
#'
#'
#' @param df
#' 
#' @return 




# This function needs to be passed to plotly::layout as follows:
# df <- data.frame(var = rep(c("A","B","C","D"), each = 20),
#                   value = rnorm(80),
#                   time = rep(c(1:20),4)) %>%
#   tidyr::spread(var, value)
# 
# p <- plot_ly(type = 'scatter', mode = 'lines')
# plotvars <- names(df)  [2: ncol(df)]
# for (i in 1:length(plotvars)){
#   temp <- df %>%
#     rename(data=one_of(plotvars [i] )) %>%
#     select(time, data)
#   
#   p <- p %>%
#     add_lines(data=temp, x=~time, y=~data, name=i) 
# }
# 
# buttons <- create_buttons(df)
# 
# p %>% 
#   layout(updatemenus = buttons)


create_buttons <- function(df){
  
  plotvars <- names(df)[2:ncol(df)]
  
  
  base_params <- 'list(
    list(
      type = "buttons",
      direction = "right",
      xanchor = "center",
      yanchor = "top",
      pad = list("r"= 0, "t"= 10, "b" = 10),
      x = 0.5,
      y = -0.2,
      buttons = list(
  %slist(
                label = "All",
                method = "update",
                args = list(list(visible = c(T, T, T, T)),
                list(title = "All"))
          )
        )
      )
    )'

  menu <- ""
  for (i in 1:length(plotvars)){
    #Create logical statement for which series to view on click
    col_id <- grep(plotvars[i], colnames(df))
    vis_logical <- c(F, rep(NA, length(plotvars)))
    vis_logical[col_id] <- T
    vis_logical[is.na(vis_logical)] <- F
    vis_logical <- paste0("c(",stringr::str_flatten(vis_logical, ","),")")
    
    menu_item <- sprintf('
      list(
        label = "%s",
        method = "update",
        args = list(list(visible = %s),
                    list(title = "%s")))',plotvars[i],
                         vis_logical,
                         plotvars[i])
    
    
    menu <- stringr::str_glue(stringr::str_glue(menu,menu_item),",")
    
    
  }
  
  out <- eval(parse(text = sprintf(base_params, menu)))
  
  return(out)
}

