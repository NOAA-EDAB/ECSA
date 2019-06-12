gls_summary <- function(mod){
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
    out %<>% mutate(`Corr.Struct.` = "ARMA(2,0)")
  } else if (length(mod$modelStruct$corStruct) == 1){
    out %<>% mutate(`Corr.Struct.` = "ARMA(1,0)")
  } 
  
  out %<>% tidyr::gather(Summary, Value) 
  
  return(out)
}