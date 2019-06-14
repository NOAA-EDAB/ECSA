#' Calculate Probability of Occurance
#'
#' @param df 
#'
#' @return dataframe of probabilities with upper and lower CI
#'
#' @examples
#' 
#' df <- data.frame(A = 1:20, pres = sample(c(TRUE,FALSE), 20, TRUE))
#' calc_p(df = df)
#' 
calc_p <- function(df) {
  m <- 
    stats::glm(formula = pres ~ 1,
        data = df, 
        family = binomial)
  
  preds <-
    stats::predict(object = m,
            type = "link", 
            se.fit = TRUE,
            newdata = df[1,])
  
  upr <- preds$fit + 1.96*preds$se.fit
  lwr <- preds$fit - 1.96*preds$se.fit
  fit <- preds$fit
  
  p      <- m$family$linkinv(fit)
  p_high <- m$family$linkinv(upr)
  p_low  <- m$family$linkinv(lwr)
  
  data.frame(p, p_high, p_low)
}