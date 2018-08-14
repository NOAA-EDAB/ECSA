# Code to calculate the probability of occurence for the NMFS
# bottom trawl data. The model is a logistic regression.
library(dplyr)
library(ggplot2)

# Function to calculate probability of occurence
calc_p <- function(df) {
  m <- 
    glm(formula = pres ~ 1,
        data = df, 
        family = binomial)
  
  preds <-
    predict(object = m,
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

load("df_survdat.rda")

strata_fall <- c(1010, 1050, 1090, 1610, 1650, 1690,
                 1730, 3010, 3020, 3030, 3040, 3050,
                 3060, 3070, 3080, 3090, 3100, 3110,
                 3120, 3130, 3140, 3150, 3160, 3170,
                 3180, 3190, 3200, 3210, 3220, 3230,
                 3240, 3250, 3260, 3270, 3280, 3290,
                 3300, 3310, 3320, 3330, 3340, 3350,
                 3360, 3370, 3380, 3390, 3400, 3410,
                 3420, 3430, 3440, 3450, 3460, 3470,
                 3480, 3490, 3500, 3510, 3520, 3530,
                 3540, 3550, 3560, 3570, 3580, 3590,
                 3600, 3610)

strata_spring <- c(1010, 1020, 1030, 1040, 1050, 1060,
                   1070, 1080, 1090, 1100, 1110, 1120,
                   1610, 1620, 1630, 1640, 1650, 1660,
                   1670, 1680, 1690, 1700, 1710, 1720,
                   1730, 1740, 1750, 1760)

df_survdat_spring <-
  df_survdat %>%
  dplyr::filter(YEAR > 1976,
                SEASON == "SPRING",
                STRATUM %in% strata_spring)

df_survdat_fall <-
  df_survdat %>%
  dplyr::filter(YEAR > 1976,
                SEASON == "FALL",
                STRATUM %in% strata_fall)

df_survdat2use <-
  rbind(df_survdat_spring,
        df_survdat_fall) %>%
  dplyr::mutate(pres = BIOMASS > 0)


df_prob_occ <-
  df_survdat2use %>%
  dplyr::group_by(SEASON, YEAR) %>%
  dplyr::do(calc_p(df = .))

# Make plot
ggplot(df_prob_occ, 
       aes(x = YEAR, y = p, color = SEASON)) +
  geom_point() +
  geom_errorbar(aes(ymin = p_low, ymax = p_high), 
                size = 0.3, width = 0.3) +
  geom_line() +
  xlab("Year") +
  ylab("Probability of encountering summer flounder") +
  theme_bw()  +
  theme(axis.title = element_text(size = 13),
        strip.text = element_text(size = 10))

