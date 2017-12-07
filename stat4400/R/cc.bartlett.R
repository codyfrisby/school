cc.bartlett <- function(x,y){ # can be used to test cannonical corr significance
  x <- as.matrix(x)
  y <- as.matrix(y)
  n <- dim(x)[1]
  q1 <- dim(x)[2]
  q2 <- dim(y)[2]
  q <- min(q1, q2)
  lam <- cancor(x,y)$cor^2
  if (q > 1){
    STAT <- df <- numeric(length = q)
    for (i in 1:q){
      STAT[i] <- -1 * (n - 0.5 * (q1 + q2 + 1)) * sum(log(1 - lam[i:q]))
      df[i] <- q1 * q2
      q1 <- q1 - 1 # knock off a degree of freedom
      q2 <- q2 - 1 # knock off a degree of freedom
    }
  }
  else {
    STAT <- -1 * (n - 0.5 * (q1 + q2 + 1)) * sum(log(1 - lam))
    df <- q1 * q2
  }
  p <- round(1 - pchisq(STAT, df = df), 6)
  return(cbind(rho = sqrt(lam), Bartlett = STAT, df = df, pValue = p))
}