n <- 20*100; b <- 10
x <- rnorm(n*b)
a <- matrix(sample(x, n*b, replace=TRUE), n, b)
f <- function(x){t.test(x)$conf.int} #create t conf intervals
d <- t(apply(a, 1, f)) # n t.test confidence intervals
# ^ bootstrap function
# what if we only have one sample from the normal?
B <- matrix(sample(a[1,], n*b, replace = TRUE), b, n)
B <- matrix(sample(x, n*b, replace = TRUE), b, n)
#bootstrap n samples n times
h <- function(x){quantile(x, c(0.025, 0.975))}
j <- t(apply(B,2,h)) #bootstrap conf intervals
y <- sign(d[,1]) * sign(d[,2])
t.accuracy <- sum(y<0) / length(y)
z <- sign(j[,1]) * sign(j[,2])
boot.accuracy <- sum(z<0) / length(z)
# Trying some other things, bootstrapping my n "synthetic" samples, size 10
g <- function(x){sample(x, b, replace = TRUE)}
C <- matrix(apply(B, 2, g), b, b)
jj <- t(apply(C,2,h)) #bootstrap conf intervals
zz <- sign(jj[,1]) * sign(jj[,2])
boot.zz <- sum(zz<0) / length(zz)
# compare results:
print(boot.accuracy); print(boot.zz); print(t.accuracy)
if(boot.accuracy < boot.zz) {
  print("zz wins!")
  } else print("easy boot wins!")

rm(list = ls())