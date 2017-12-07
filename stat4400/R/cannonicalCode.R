# using some of the functions from CCA:
df <- read.csv("~/Documents/STAT4400/data/headsize.csv")
x <- X <- cbind(df$head1, df$breadth1)
y <- Y <- cbind(df$head2, df$breadth2)
################
## CCA::cc <- function (X, Y) 
Xnames = dimnames(X)[[2]]
Ynames = dimnames(Y)[[2]]
ind.names = dimnames(X)[[1]]
res = CCA::rcc(X, Y, 0, 0)
#############
## CCA::rcc <- function (X, Y, lambda1, lambda2) 
lambda1 <- 0; lambda2 <- 0
Xnames <- dimnames(X)[[2]]
Ynames <- dimnames(Y)[[2]]
ind.names <- dimnames(X)[[1]]
Cxx <- var(X, na.rm = TRUE, use = "pairwise") + diag(lambda1, 
                                                     ncol(X))
Cyy <- var(Y, na.rm = TRUE, use = "pairwise") + diag(lambda2, 
                                                     ncol(Y))
Cxy <- cov(X, Y, use = "pairwise")
res <- fda::geigen(Cxy, Cxx, Cyy)
names(res) <- c("cor", "xcoef", "ycoef")
scores <- CCA::comput(X, Y, res)
##########
Amat <- Cxy
Bmat <- Cxx
Cmat <- Cyy
#fda::geigen <- function (Amat, Bmat, Cmat) 
Bdim <- dim(Bmat)
Cdim <- dim(Cmat)
p <- Bdim[1]
q <- Cdim[1]
s <- min(c(p, q))
Bfac <- chol(Bmat)
Cfac <- chol(Cmat)
Bfacinv <- solve(Bfac)
Cfacinv <- solve(Cfac)
Dmat <- t(Bfacinv) %*% Amat %*% Cfacinv
if (p >= q) {
  result <- svd2(Dmat)
  values <- result$d
  Lmat <- Bfacinv %*% result$u
  Mmat <- Cfacinv %*% result$v
}
else {
  result <- svd2(t(Dmat))
  values <- result$d
  Lmat <- Bfacinv %*% result$v
  Mmat <- Cfacinv %*% result$u
}
geigenlist <- list(values, Lmat, Mmat)
names(geigenlist) <- c("values", "Lmat", "Mmat")
#################
### CCA::comput <- function (X, Y, res) 
X.aux = scale(X, center = TRUE, scale = FALSE)
Y.aux = scale(Y, center = TRUE, scale = FALSE)
X.aux[is.na(X.aux)] = 0
Y.aux[is.na(Y.aux)] = 0
xscores = X.aux %*% res$xcoef
yscores = Y.aux %*% res$ycoef
corr.X.xscores = cor(X, xscores, use = "pairwise")
corr.Y.xscores = cor(Y, xscores, use = "pairwise")
corr.X.yscores = cor(X, yscores, use = "pairwise")
corr.Y.yscores = cor(Y, yscores, use = "pairwise")
### and to test for significant canonical correlations:
# R function to test for statistically significant canonical dimensions
cc.sig <- function(x, y) { # SAS does a similar test in PROC CANCORR
  (cc1 <- cancor(x, y))
  (ev <- (1 - cc1$cor^2)) # one minus the eigen values
  (n <- dim(x)[1])
  (p <- dim(x)[2])
  (q <- dim(y)[2])
  (k <- min(p, q))
  (m <- n - 3/2 - (p + q)/2)
  (w <- rev(cumprod(rev(ev))))
  # initialize
  (d1 <- d2 <- f <- vector("numeric", k))
  for (i in 1:k) {
    (s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5)))
    (si <- 1/s)
    (d1[i] <- p * q)
    (d2[i] <- m * s - p * q/2 + 1)
    (r <- (1 - w[i]^si)/w[i]^si)
    (f[i] <- r * d2[i]/d1[i])
    (p <- p - 1)
    (q <- q - 1)
  }
  pv <- round(pf(f, d1, d2, lower.tail = FALSE), 6) # 6 sig figs
  dmat <- cbind(cor = cc1$cor, WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
  return(dmat)
}
# test for significance
mm <- read.csv("http://www.ats.ucla.edu/stat/data/mmreg.csv")
colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write",
                  "Math", "Science", "Sex")
psych <- mm[, 1:3]
acad <- mm[, 4:8]
source("~/Documents/STAT4400/R/cc.sig.R")
cc.sig(psych, acad)
#Above is essentially what SAS PROC cancorr is doing

# Below is what SAS is NOT doing:
df <- read.csv("~/Documents/STAT4400/data/headsize.csv")
df.sd <- scale(df) # scaling just to check it out.
# this is what sweep is doing:
df[,1]/sd(df[,1]) # same thing, sweep just does it to all the columns
# example from page 97
(r <- cor(df.sd))
r11 <- r[1:2, 1:2]; r12 <- r[1:2, 3:4]
r22 <- r[3:4, 3:4]; r21 <- r[3:4, 1:2]
(E1 <- solve(r11) %*% r12 %*% solve(r22) %*% r21)
(E2 <- solve(r22) %*% r21 %*% solve(r11) %*% r12)
(e1 <- eigen(E1))
(e2 <- eigen(E2))


#####  stats package function cancor
function (x, y, xcenter = TRUE, ycenter = TRUE) 
{
  x <- as.matrix(x)
  y <- as.matrix(y)
  if ((nr <- nrow(x)) != nrow(y)) 
    stop("unequal number of rows in 'cancor'")
  ncx <- ncol(x)
  ncy <- ncol(y)
  if (!nr || !ncx || !ncy) 
    stop("dimension 0 in 'x' or 'y'")
  if (is.logical(xcenter)) {
    if (xcenter) {
      xcenter <- colMeans(x, )
      x <- x - rep(xcenter, rep.int(nr, ncx))
    }
    else xcenter <- rep.int(0, ncx)
  }
  else {
    xcenter <- rep_len(xcenter, ncx)
    x <- x - rep(xcenter, rep.int(nr, ncx))
  }
  if (is.logical(ycenter)) {
    if (ycenter) {
      ycenter <- colMeans(y)
      y <- y - rep(ycenter, rep.int(nr, ncy))
    }
    else ycenter <- rep.int(0, ncy)
  }
  else {
    ycenter <- rep_len(ycenter, ncy)
    y <- y - rep(ycenter, rep.int(nr, ncy))
  }
  (qx <- qr(x))
  (qy <- qr(y))
  (dx <- qx$rank)
  if (!dx) 
    stop("'x' has rank 0")
  (dy <- qy$rank)
  if (!dy) 
    stop("'y' has rank 0")
  (z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1L:dx, , 
                                                  drop = FALSE], dx, dy))
  (xcoef <- backsolve((qx$qr)[1L:dx, 1L:dx, drop = FALSE], z$u))
  rownames(xcoef) <- colnames(x)[qx$pivot][1L:dx]
  (ycoef <- backsolve((qy$qr)[1L:dy, 1L:dy, drop = FALSE], z$v))
  rownames(ycoef) <- colnames(y)[qy$pivot][1L:dy]
  list(cor = z$d, xcoef = xcoef, ycoef = ycoef, xcenter = xcenter, 
       ycenter = ycenter)
}