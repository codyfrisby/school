# R function to test for statistically significant canonical dimensions
cc.wilks <- function(x, y) { # SAS does a similar test in PROC CANCORR
  x <- as.matrix(x)
  y <- as.matrix(y)
  cc1 <- cancor(x, y)
  ev <- (1 - cc1$cor^2) # one minus the eigen values
  n <- dim(x)[1]
  p <- dim(x)[2]
  q <- dim(y)[2]
  k <- min(p, q)
  m <- n - 3/2 - (p + q)/2
  w <- rev(cumprod(rev(ev)))
  # initialize
  d1 <- d2 <- f <- vector("numeric", k)
  for (i in 1:k) {
    s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    if (is.nan(s)) s <- 1 # added this line, not sure if it's OK but delivers similar results to CCP::p.asym
    si <- 1/s
    d1[i] <- p * q
    d2[i] <- m * s - p * q/2 + 1
    r <- (1 - w[i]^si)/w[i]^si
    f[i] <- r * d2[i]/d1[i]
    p <- p - 1 # knock off a degree of freedom for the next test
    q <- q - 1 # same, a loop is an easy way to handle the changing degrees of freedom for this test
  }
  pv <- round(pf(f, d1, d2, lower.tail = FALSE), 6) # 6 sig figs
  dmat <- cbind(rho = cc1$cor, WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
  return(dmat)
}