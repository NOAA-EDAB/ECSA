#' Summarizes GLS fits
#'
#' Uses output object from nlme and customizes summary stats
#'
#' @param mod object from nlme fit
#'
#' @return data frame
#' 
#' \item{Trend}{Estimate of trend intercept and slope}
#' \item{Lower 95%CI}{Lower 95% confidence interval for the slope}
#' \item{Upper 95%CI}{Upper 95% confidence interval for the slope}
#' \item{P}{Pvalue for significance of the slope}
#' \item{N}{Number of data points used in fitting trend and assessing significance}
#' \item{Corr.Struct.}{error correlation structure}
#' 
#' @importFrom magrittr "%<>%"
#'

gls_summary <- function(mod){
  
  `%<>%` <- magrittr::`%<>%`
  
  intercept <- round(coef(mod)[1],3)
  trend <- round(coef(mod)[2],3)
  lower <- round(nlme::intervals(mod)$coef[2],3)
  upper <- round(nlme::intervals(mod)$coef[6],3)
  summ <- summary(mod)
  N <- summ$dims$N
  res.std.err <- round(summ$sigma,3)
  P <- round(summ$tTable[8],3)
  
  out <- data.frame(Trend = trend,
                    `Lower 95%CI` = lower,
                    `Upper 95%CI` = upper,
                    P = P,
                    N = N)
  
  if (length(mod$modelStruct$corStruct) == 2){
    out %<>% dplyr::mutate(`Corr.Struct.` = "ARMA(2,0)")
  } else if (length(mod$modelStruct$corStruct) == 1){
    out %<>% dplyr::mutate(`Corr.Struct.` = "ARMA(1,0)")
  } 
  
  out %<>% tidyr::gather(Summary, Value) 
  
  return(out)
}