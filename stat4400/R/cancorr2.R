cancor2<-function(x,y,dec=4){
  #Canonical Correlation Analysis to mimic SAS PROC CANCOR output.
  #Basic formulas can be found in Chapter 10 of Mardia, Kent, and Bibby (1979).
  # The approximate F statistic is exercise 3.7.6b.
  x <- as.matrix(x)
  y <- as.matrix(y)
  n <- dim(x)[1]
  q1 <- dim(x)[2]
  q2 <- dim(y)[2]
  q <- min(q1,q2)
  S11 <- cov(x)
  S12 <- cov(x,y)
  S21 <- t(S12)
  S22 <- cov(y)
  E1 <- eigen(solve(S11) %*% S12 %*% solve(S22) %*% S21)
  E2 <- eigen(solve(S22) %*% S21 %*% solve(S11) %*% S12)
  rsquared <- E1$values[1:q]
  LR <- NULL;pp<-NULL;qq<-NULL;tt<-NULL
  for (i in 1:q){
    LR <- c(LR, prod(1 - rsquared[i:q]))
    pp <- c(pp, q1 - i + 1)
    qq <- c(qq, q2 - i + 1)
    tt <- c(tt, n - 1 - i + 1)}
  m <- tt - 0.5 * (pp + qq + 1)
  lambda <- (1/4) * (pp * qq - 2)
  s <- sqrt((pp^2 * qq^2 - 4) / (pp^2 + qq^2 - 5))
  f <- ((m * s - 2 * lambda) / (pp * qq)) * ((1 - LR^(1/s)) / LR^(1/s))
  df1 <- pp * qq
  df2 <- (m * s - 2*lambda)
  pval <- 1 - pf(f, df1, df2)
  outmat <- round(cbind(sqrt(rsquared), rsquared, LR, f, df1, df2, pval),dec)
  colnames(outmat) = list("R","RSquared","LR","ApproxF","NumDF","DenDF","pvalue")
  rownames(outmat) = as.character(1:q);xrels<-round(cor(x,x%*%E1$vectors)[,1:q],dec)
  colnames(xrels) <- apply(cbind(rep("U",q),as.character(1:q)),1,paste,collapse="")
  yrels <- round(cor(y,y%*%E2$vectors)[,1:q],dec)
  colnames(yrels)<-apply(cbind(rep("V",q),as.character(1:q)),1,paste,collapse="")
  list(Summary=outmat,a.Coefficients=E1$vectors,b.Coefficients=E2$vectors,
       XUCorrelations=xrels,YVCorrelations=yrels)
}