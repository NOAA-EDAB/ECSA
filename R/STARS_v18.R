#' STARS_algorithm
#' 
#' Describe
#' 
#' @param mat.in A matrix with the time values in column 1 and data vector in column 2. 
#' @param window.L STARS window length.
#' @param sub.sample.size Sub-sample size.
#' @param huber.wt.val Huber weight.
#' @param alpha.level Alpha level.
#' 
#' @return 
#' 
#' 

STARS = function(mat.in = c(),
                 #these for blooms
                 window.L = 5,       # 5 for blooms  | 10 for bts change pt
                 sub.sample.size = 3, # 3
                 huber.wt.val = 3,
                 alpha.level= 0.10   ) {

                 # these for bt st
                 #window.L = 10,       # 5 for blooms  | 10 for bts change pt
                 #sub.sample.size = 6, # 3
                 #huber.wt.val = 1,
                 #alpha.level= 0.10   ) {

# Declare vector to analyze
  data.vect <- mat.in[,2]

  # minimum sub.sample.size is 3
  if (sub.sample.size <= 2) {sub.sample.size <- 3}

  # 2. Determine rho value
  AR.estimate.function <- function(d.vect = data.vect,
                                   sample.length = sub.sample.size) {

    if (any(duplicated(d.vect))) {
      fix.ind <- which(duplicated(d.vect))
      length.ind <- length(fix.ind)
      rand.dev <- base::sample(x = c(-1,1), size = length.ind, replace = T)*stats::runif(length.ind,min = 0.001,max = 0.002)
      d.vect[fix.ind] <- d.vect[fix.ind] + rand.dev  }

    start.ind <- seq(1,c(length(d.vect) - sample.length + 1))
    AR.est <- c()
    for (k in 1:length(start.ind)) {
      ind <- seq(start.ind[k],length.out = sample.length)
      if (suppressWarnings(length(ar.ols(d.vect[ind],order.max = 1)$ar)) > 0) {
        AR.est[k] <- suppressWarnings(ar.ols(d.vect[ind],order.max = 1)$ar[[1]]) }}
    AR.est <- median(AR.est,na.rm = T)
    return(AR.est)  }
  
  
  AR.est <- AR.estimate.function(d.vect = data.vect, sample.length = sub.sample.size)

  # Calculate average variance of TS
  determine.VAR <- function(d.vect = mat.in[,2],length.val = window.L) {
    series.len <- length(d.vect)
    start.ind <- seq(1,series.len-length.val+1)
    VAR.est <- c()
    for (j in 1:length(start.ind)) {
      VAR.est[j] <- var(d.vect[seq(start.ind[j],length.out = length.val)],na.rm = T)  }
    VAR.est <- mean(VAR.est)
    return(VAR.est)  }
  
  VAR.est <- determine.VAR()

  # Huber wt function
  huber.wt.mean <- function(vector.in, huber.wt = huber.wt.val, VAR.est) {
    wt.val <- rep(NA,length(vector.in))
    anom.det <- (abs(vector.in - mean(vector.in)))/VAR.est^(0.5)
    one.wt <- which(anom.det <= huber.wt)
    if (length(one.wt) >= 1) { wt.val[one.wt] <- 1 }
    red.wt <- which(anom.det > huber.wt)
    if (length(red.wt) >= 1) { wt.val[red.wt] <- 1/anom.det[red.wt] }
    return(wt.val)  }

  Eq.sample.size = function(AR.val = AR.est,
                            Ns = window.L) {
    sum.val <- 0
    for (ZZZ in 1:c(Ns-1)) { sum.val <- sum.val + (1 - (ZZZ/Ns))*AR.est^ZZZ }
    EqN <- Ns/(1 + sum.val)
    if (EqN <= 2) { EqN <- 2 }
    if (EqN > Ns) { EqN <- Ns }
    return(EqN) }

  # is value in the interval?
  in_interval <- function(x, interval) {
    stopifnot(length(interval) == 2L)
    interval[1] < x & x < interval[2] }

  # regime bounds
  # (df.val <- 2*Eq.sample.size(AR.val = AR.est,Ns = window.L) - 2)
  # (diff.val <- qt(c(1 - alpha.level/2),df.val)*(sqrt((2*VAR.est)/window.L)))
  (df.val <- Eq.sample.size(AR.val = AR.est,Ns = 2*window.L - 2))
  (diff.val <- qt(c(1 - alpha.level/2),floor(df.val))*(sqrt((2*VAR.est)/window.L)))

  # Initialize vector for mean value, reg.number, RSI.vect
  regime.mean.vect <- rep(NA,length(data.vect))
  reg.number.vect <- rep(NA,length(data.vect))
  RSI.vect <- rep(NA,length(data.vect))

  regime.mean.vect[1:window.L] <- stats::weighted.mean(data.vect[1:window.L],
                                                huber.wt.mean(data.vect[1:window.L],
                                                              huber.wt = huber.wt.val,
                                                              VAR.est))
  reg.number.vect[1] <- 1
  RSI.vect[1] <- 0

  cbind(data.vect,regime.mean.vect,reg.number.vect,RSI.vect)

  while (length(which(is.na(reg.number.vect))) > 0) {

    ind.pot.reg  <- which(is.na(reg.number.vect))[1]
    reg.number.vect[ind.pot.reg] <- reg.number.vect[ind.pot.reg - 1] + 1

    range.val <- c(regime.mean.vect[ind.pot.reg-1] - diff.val,
                   regime.mean.vect[ind.pot.reg-1] + diff.val)

    in.check <- in_interval(data.vect[ind.pot.reg],range.val)

    # No shift detected
    if (in.check) {
      prv.regime.ind <- sort(c(which(reg.number.vect == reg.number.vect[ind.pot.reg] - 1),
                               which(reg.number.vect == reg.number.vect[ind.pot.reg])),decreasing = T)

      if (length(which(is.na(regime.mean.vect[prv.regime.ind]))) != 0) {
        regime.mean.vect[prv.regime.ind] <- stats::weighted.mean(data.vect[prv.regime.ind],
                                                          huber.wt.mean(data.vect[prv.regime.ind],
                                                                        huber.wt = huber.wt.val,
                                                                        VAR.est))   }

      reg.number.vect[ind.pot.reg] <- reg.number.vect[ind.pot.reg - 1]
      RSI.vect[ind.pot.reg] <- 0
      (cbind(data.vect,regime.mean.vect,reg.number.vect,RSI.vect))    }

    # Shift detected
    if (!in.check) {
      up.dn.ind <- which(c(data.vect[ind.pot.reg] > range.val[2],
                           data.vect[ind.pot.reg] < range.val[1]))

      pot.reg.ind <- seq(ind.pot.reg,length.out = window.L)
      term.ind <- which(pot.reg.ind > length(data.vect))
      if (length(term.ind) > 0) { pot.reg.ind <- pot.reg.ind[-term.ind] }

      # reg.up
      if (up.dn.ind == 1) { x.star <- data.vect[pot.reg.ind] - range.val[2] }
      # reg.dn
      if (up.dn.ind == 2) { x.star <-  range.val[1] - data.vect[pot.reg.ind] }

      RSI.vals <- cumsum(x.star/(window.L*sqrt(VAR.est)))

      if (all(RSI.vals > 0)) {
        reg.1.wt <- huber.wt.mean(vector.in = data.vect[pot.reg.ind],
                                  huber.wt.val,
                                  VAR.est)

        reg.1.mean <- stats::weighted.mean(data.vect[pot.reg.ind], reg.1.wt)
        regime.mean.vect[pot.reg.ind] <- reg.1.mean

        if (length(RSI.vals) == 1) {
          regime.mean.vect[pot.reg.ind] <- data.vect[pot.reg.ind]
          RSI.vect[ind.pot.reg] <- RSI.vals[length(RSI.vals)]
        }

        if (length(RSI.vals) > 1) {
          RSI.vect[ind.pot.reg] <- RSI.vals[length(RSI.vals)]
        }

        # Recalculate old regime mean
        old.reg.ind <- which(reg.number.vect == reg.number.vect[ind.pot.reg - 1])
        regime.mean.vect[old.reg.ind] <- stats::weighted.mean(data.vect[old.reg.ind],
                                                       huber.wt.mean(data.vect[old.reg.ind],
                                                                     huber.wt = huber.wt.val,
                                                                     VAR.est))  }

      if (any(RSI.vals < 0)) {
        prv.regime.ind <- sort(c(which(reg.number.vect == reg.number.vect[ind.pot.reg] - 1),
                                 which(reg.number.vect == reg.number.vect[ind.pot.reg])),decreasing = T)

        if (length(which(is.na(regime.mean.vect[prv.regime.ind]))) != 0) {
          regime.mean.vect[prv.regime.ind] <- stats::weighted.mean(data.vect[prv.regime.ind],
                                                            huber.wt.mean(data.vect[prv.regime.ind],
                                                                          huber.wt = huber.wt.val,
                                                                          VAR.est))   }

        reg.number.vect[ind.pot.reg] <- reg.number.vect[ind.pot.reg - 1]
        RSI.vect[ind.pot.reg] <- 0
      }
    } # in.check loop


  } # while loop

  STARS.dat <- cbind(mat.in,regime.mean.vect,reg.number.vect,RSI.vect)
  STARS.dat <- as.data.frame(STARS.dat)
  names(STARS.dat) <- c("Time","Data","Regime.Mean","Regime.Number","RSI")

  if (length(sort(unique(STARS.dat$Regime.Number))) > 1) {
    un.reg <- sort(unique(STARS.dat$Regime.Number))
    un.reg <- un.reg[2:length(un.reg)]
    for (Z in un.reg) {
      ind.2 <- which(STARS.dat$Regime.Number == Z)[1]
      ind.1 <- ind.2 - 1
      if (STARS.dat$Regime.Mean[ind.1] > STARS.dat$Regime.Mean[ind.2]) {
        STARS.dat$RSI[ind.2] <- (-1)*STARS.dat$RSI[ind.2]
      }
    }
  }

  return(STARS.dat)

}
