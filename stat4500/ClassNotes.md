# STAT 4500 Class Notes
## This document is an R markdown file of the class notes from STAT 4500, Non Parametric Statistics for Fall 2016 at UVU.  



## 8/24/16 Notes: 
-Defined Random variable  
-PMF  
-PDF  
-Ranks  

## Binomial Distribution Discussion 8/26/16:  


```r
# 8/26/16 Non Parametric class notes:
x <- 0:10
dbinom(x, 10, 0.5) #dbinom is the PMF of binomial
```

```
##  [1] 0.0009765625 0.0097656250 0.0439453125 0.1171875000 0.2050781250
##  [6] 0.2460937500 0.2050781250 0.1171875000 0.0439453125 0.0097656250
## [11] 0.0009765625
```

```r
pbinom(x,10,0.5, lower.tail = FALSE) # this is for cummulative
```

```
##  [1] 0.9990234375 0.9892578125 0.9453125000 0.8281250000 0.6230468750
##  [6] 0.3769531250 0.1718750000 0.0546875000 0.0107421875 0.0009765625
## [11] 0.0000000000
```

### There is a special type of test called "Randomized Test"  
read about it on page 27-28 in text book


```r
# what is the probability that x = 8 under the null
pbinom(8,10,0.5)
```

```
## [1] 0.9892578
```

```r
0.0393/pbinom(8,10,0.5)
```

```
## [1] 0.03972675
```

Reject with probability 1 is sum(x_i) > 8  
Reject with probability 0.0397 if sum(x_i) = 8  
Reject with probability 0 if sum(x_i) < 8  

## 8/31/16 Power Curve Example  


```r
# power example from class
power <- function(x){pnorm(10.645 - x, lower.tail = F)}
beta <- function(x){1-pnorm(10.645 - x, lower.tail = F)}
curve(power, 9, 15, col="blue", lwd=2)
curve(beta, 9, 15, add=T, col="red", lwd=2)
```

<img src="ClassNotes_files/figure-html/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

## Sign Test  
Assumption: f is continuous.  $\theta$ is the median.  
$H_0: \theta = \theta_0$ vs. $H_1: \theta > \theta_0$.  

Example 2.2 (page 27) from the text: $H_0: \theta = 200$ vs. 
$H_1: \theta \neq 200$


```r
# data from the example
y <- c(49,58,75,110,112,132,151,276,281,362)
# binomial with n = 10, p = 0.5
pbinom(0:10, 10, 0.5)
```

```
##  [1] 0.0009765625 0.0107421875 0.0546875000 0.1718750000 0.3769531250
##  [6] 0.6230468750 0.8281250000 0.9453125000 0.9892578125 0.9990234375
## [11] 1.0000000000
```

```r
1 - pbinom(0:10, 10, 0.5)
```

```
##  [1] 0.9990234375 0.9892578125 0.9453125000 0.8281250000 0.6230468750
##  [6] 0.3769531250 0.1718750000 0.0546875000 0.0107421875 0.0009765625
## [11] 0.0000000000
```

```r
plot(dbinom(0:10, 10, 0.5), type="h", lwd=2)
```

<img src="ClassNotes_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

## 9/2/16  


```r
# example from class where H0 = 0.5, H1 < 0.5
p0 <- 0.5
p1 <- 0.25
n <- 20
prob <- pbinom(0:n, n, p0)
prob # critical value is n=1
```

```
##  [1] 9.536743e-07 2.002716e-05 2.012253e-04 1.288414e-03 5.908966e-03
##  [6] 2.069473e-02 5.765915e-02 1.315880e-01 2.517223e-01 4.119015e-01
## [11] 5.880985e-01 7.482777e-01 8.684120e-01 9.423409e-01 9.793053e-01
## [16] 9.940910e-01 9.987116e-01 9.997988e-01 9.999800e-01 9.999990e-01
## [21] 1.000000e+00
```

```r
cv <- max(which(prob <= 0.05) - 1) # gives us critical value of x
cv
```

```
## [1] 5
```

```r
# what's the power of this test?
pow <- pbinom(cv, n, p1)
pow
```

```
## [1] 0.6171727
```

```r
# let's create a function called power
power <- function(n, p0=0.5, p1){
  prob <- pbinom(0:n, n, p0)
  cv <- max(which(prob <= 0.05)-1)
  pow <- pbinom(cv,n,p1)
  return(pow)
} # this function needs an n, which needs to be a scalar not a vector, and p0, p1
# try to plot this
x <- vector()
for(i in 10:100){
  x[i] <- power(i, p0=0.5, p1=0.25)
}
x <- x[-(1:9)] # get rid of NAs
plot(10:100, x, main="Power for p1 = 0.25", xlab="n", ylab="Power", pch=16)
```

<img src="ClassNotes_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />


### Relative Efficiency  

T1 and T2.  At some fixed level $\alpha$   

For T1, let n1 be the sample size required to achieve a type II error $\beta$.  
For T2, let n2 be the sample size required to achieve the same type II error
$\beta$.
Relative efficiency of T2 with respect to T1 is $\frac{n_1}{n_2}$.  

Sign test with respect to t test  $$\frac{2}{\pi} < 1$$  


```r
# data from page 48
x <- c(73,82,87,68,106,60,97)
# let's sort
y <- x[order(x)]
# Empical distribution from class
n <- length(x)
Fn <- NULL
for(i in 1:n){
  Fn[i] <- i/n
}
plot(y, Fn, type="s")
```

<img src="ClassNotes_files/figure-html/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

```r
# another example
# generate data from the uniform distribution
x <- runif(50,2,7)
y <- x[order(x)]
# Empical CDF
n <- length(x)
Fn <- 1:n/n
plot(y, Fn, type="s") # Empiracle distribution
curve((x-2)/5,2,7,add=T) # actual distribution
curve(1 - exp(-x),2,7,add = TRUE, col="blue") # not sure what this one is
```

<img src="ClassNotes_files/figure-html/unnamed-chunk-6-2.png" style="display: block; margin: auto;" />

```r
# These are all aspects of exploratory data analysis
```

## Tests for Median 9/7/16  

(i) Sign test
(ii) Wilcoxon signed rank test
  - Data is continuous
  - Data is symmetric about the median
Difference between these two tests is that Wilcoxon test takes into account BOTH the sign AND the rank.  Sign test considers only the sign. 

Hypotheses for Wilcoxon test:
$$H_0: \theta = \theta_0 ~ vs ~ H_1 \theta \neq \theta_0$$
$$H_0: \theta = \theta_0 ~ vs ~ H_1 \theta > \theta_0$$
$$H_0: \theta = \theta_0 ~ vs ~ H_1 \theta < \theta_0$$  
We have data ordered, from smallest to largest:

$$X_1, X_2, X_3, ..., X_n$$

Calculate $d_i$ which is equal to $|X_i - \theta_0|$

### Heart rate example again:


```r
x <- c(73,82,87,68,106,60,97)
# how many are less than or equal to 69?
length(which(x <= 69))
```

```
## [1] 2
```

```r
# p of x
length(which(x <= 69))/length(x)
```

```
## [1] 0.2857143
```

## Using R to assign ranks and signs to a vector:  


```r
# using the vector x from above:
x <- c(73,82,87,68,106,60,97)
sign <- ifelse(x > 70, 1, -1) # if H0 was 70
d <- rank(abs(x - 70))
rs <- sign*d
cbind(x, sign, d, rs)
```

```
##        x sign d rs
## [1,]  73    1 2  2
## [2,]  82    1 4  4
## [3,]  87    1 5  5
## [4,]  68   -1 1 -1
## [5,] 106    1 7  7
## [6,]  60   -1 3 -3
## [7,]  97    1 6  6
```

```r
sum(ifelse(rs>0, rs, 0))
```

```
## [1] 24
```


## Notes on page 47-48, developing the sign/rank test


```r
library(gtools)
sum(1:7) # sum of 1 to 7
```

```
## [1] 28
```

```r
2^7 # possible assignents of -1 or 1 to seven samples
```

```
## [1] 128
```

```r
A <- permutations(2, 7, v=c(-1,1), repeats.allowed = T) # all possible combos
head(A) # look at what A is now
```

```
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
## [1,]   -1   -1   -1   -1   -1   -1   -1
## [2,]   -1   -1   -1   -1   -1   -1    1
## [3,]   -1   -1   -1   -1   -1    1   -1
## [4,]   -1   -1   -1   -1   -1    1    1
## [5,]   -1   -1   -1   -1    1   -1   -1
## [6,]   -1   -1   -1   -1    1   -1    1
```

```r
A <- t( t(A) * 1:7) # better than a loop :)
head(A) # now what does A look like
```

```
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
## [1,]   -1   -2   -3   -4   -5   -6   -7
## [2,]   -1   -2   -3   -4   -5   -6    7
## [3,]   -1   -2   -3   -4   -5    6   -7
## [4,]   -1   -2   -3   -4   -5    6    7
## [5,]   -1   -2   -3   -4    5   -6   -7
## [6,]   -1   -2   -3   -4    5   -6    7
```

```r
# If our hypothesis involved the positives...
A <- ifelse(A > 0, A, 0) # make all negatives = 0
S <- prop.table(table(apply(A,1,sum))) # probability of each sum
S # same as table 3.1 on pg 48 in textbook
```

```
## 
##         0         1         2         3         4         5         6 
## 0.0078125 0.0078125 0.0078125 0.0156250 0.0156250 0.0234375 0.0312500 
##         7         8         9        10        11        12        13 
## 0.0390625 0.0390625 0.0468750 0.0546875 0.0546875 0.0625000 0.0625000 
##        14        15        16        17        18        19        20 
## 0.0625000 0.0625000 0.0625000 0.0546875 0.0546875 0.0468750 0.0390625 
##        21        22        23        24        25        26        27 
## 0.0390625 0.0312500 0.0234375 0.0156250 0.0156250 0.0078125 0.0078125 
##        28 
## 0.0078125
```

```r
# pg 47 - Pr(S+ <= 3)
sum(S[1:4]) # same as 5/128
```

```
## [1] 0.0390625
```

```r
plot(S, ylim = c(0,0.07), ylab="")
```

<img src="ClassNotes_files/figure-html/3.3-1.png" style="display: block; margin: auto;" />

## Writing a function for the sign/rank test:  


```r
# writing a function that replicates the book example:
# from page 48 Example 3.1
# note: this result is a one sided test.  If you want a two sided
# p value then multiply your result by two.
test <- function(x, theta){
  if(length(x) >= 20){stop("n is too large for this function")}
  sign <- ifelse(x > theta, 1, -1)
  d <- rank(abs(x - theta))
  rs <- sign*d
  t <- sum(ifelse(rs>0, rs, 0))
  A <- gtools::permutations(2, length(x), v=c(-1,1), repeats.allowed = T)
  A <- t( t(A) * 1:length(x))
  A <- ifelse(A > 0, A, 0)
  S <- table(apply(A,1,sum))
  if(t < S[(length(S)/2) + 1]){
    sum(S[1:(t+1)])/2^length(x)
  } else{
    sum(S[(t+1):length(S)])/2^length(x)
  }
}
wilcox.test(x, mu=70, alternative = "greater")$p.value
```

```
## [1] 0.0546875
```

```r
test(x,70) # same result :)
```

```
## [1] 0.0546875
```


```r
#page 51 example
x1 <- c(-2,4,8,25,-5,16,3,1,12,17,20,9)
# using the function above:
test(x1, 15) * 2 # test median is 15, 2x for two sided
```

```
## [1] 0.05224609
```

And we get the same result as the book.  

By way of reproducing the plot on page 51 in the text book:  


```r
x <- x1; theta <- 15
sign <- ifelse(x > theta, 1, -1)
d <- rank(abs(x - theta))
rs <- sign*d
t <- sum(ifelse(rs>0, rs, 0))
A <- gtools::permutations(2, length(x), v=c(-1,1), repeats.allowed = T)
A <- t( t(A) * 1:length(x))
A <- ifelse(A > 0, A, 0)
S <- table(apply(A,1,sum))
plot(S)
```

<img src="ClassNotes_files/figure-html/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

```r
Sp <- prop.table(S)[1:40] # same as page 50 table 3.2
Sp
```

```

           0            1            2            3            4 
0.0002441406 0.0002441406 0.0002441406 0.0004882812 0.0004882812 
           5            6            7            8            9 
0.0007324219 0.0009765625 0.0012207031 0.0014648438 0.0019531250 
          10           11           12           13           14 
0.0024414062 0.0029296875 0.0036621094 0.0041503906 0.0048828125 
          15           16           17           18           19 
0.0058593750 0.0065917969 0.0075683594 0.0087890625 0.0097656250 
          20           21           22           23           24 
0.0109863281 0.0124511719 0.0136718750 0.0148925781 0.0163574219 
          25           26           27           28           29 
0.0175781250 0.0190429688 0.0205078125 0.0217285156 0.0229492188 
          30           31           32           33           34 
0.0244140625 0.0253906250 0.0263671875 0.0275878906 0.0280761719 
          35           36           37           38           39 
0.0288085938 0.0295410156 0.0297851562 0.0300292969 0.0302734375 
```


```r
library(exactRankTests)
# Example 3.2
x <- c(-2,4,8,25,-5,16,3,1,12,17,20,9)
d <- x -15
si <- sign(d)
r <- rank(abs(d))
sum(r[si > 0]); sum(r[si < 0]) # S+ and S-
```

```
## [1] 14
```

```
## [1] 64
```

```r
p <- wilcox.test(x, mu=15)$p.value
p
```

```
## [1] 0.05224609
```

```r
psignrank(14, 12) * 2 # same p value, two-sided
```

```
## [1] 0.05224609
```

## Programming the Walsh averages:  


```r
x <- c(-2,4,8,25,-5,16,3,1,12,17,20,9)
y <- sort(x)
# creating Table 3.3 on page 54:
w <- outer(y,y,"+")/2
row.names(w) <- y; colnames(w) <- y
w
```

```
##      -5   -2    1    3    4    8    9   12   16   17   20   25
## -5 -5.0 -3.5 -2.0 -1.0 -0.5  1.5  2.0  3.5  5.5  6.0  7.5 10.0
## -2 -3.5 -2.0 -0.5  0.5  1.0  3.0  3.5  5.0  7.0  7.5  9.0 11.5
## 1  -2.0 -0.5  1.0  2.0  2.5  4.5  5.0  6.5  8.5  9.0 10.5 13.0
## 3  -1.0  0.5  2.0  3.0  3.5  5.5  6.0  7.5  9.5 10.0 11.5 14.0
## 4  -0.5  1.0  2.5  3.5  4.0  6.0  6.5  8.0 10.0 10.5 12.0 14.5
## 8   1.5  3.0  4.5  5.5  6.0  8.0  8.5 10.0 12.0 12.5 14.0 16.5
## 9   2.0  3.5  5.0  6.0  6.5  8.5  9.0 10.5 12.5 13.0 14.5 17.0
## 12  3.5  5.0  6.5  7.5  8.0 10.0 10.5 12.0 14.0 14.5 16.0 18.5
## 16  5.5  7.0  8.5  9.5 10.0 12.0 12.5 14.0 16.0 16.5 18.0 20.5
## 17  6.0  7.5  9.0 10.0 10.5 12.5 13.0 14.5 16.5 17.0 18.5 21.0
## 20  7.5  9.0 10.5 11.5 12.0 14.0 14.5 16.0 18.0 18.5 20.0 22.5
## 25 10.0 11.5 13.0 14.0 14.5 16.5 17.0 18.5 20.5 21.0 22.5 25.0
```



```r
# ?SignRank, ?dsignrank
par(mfrow = c(2,2))
for(n in c(4,5,10,40)) {
  x <- seq(0, n*(n+1)/2, length = 501)
  plot(x, dsignrank(x, n = n), type = "l",
       main = paste0("dsignrank(x, n = ", n, ")"))
}
```



```r
# running the van Waerden test in R with one-sample
install.packages("snpar")
library(snpar)
ns.test(x, q=theta, alternative = "greater")
```


## 9/16/16


```r
before <- c(51.2,46.5,24.1,10.2,65.3,92.1,30.3,49.2)
after <- c(45.8,41.3,15.8,11.1,58.5,70.3,31.6,35.4)
d <- before - after
wilcox.test(d, conf.int = T, alternative = "greater")
```

```
## 
## 	Wilcoxon signed rank test
## 
## data:  d
## V = 33, p-value = 0.01953
## alternative hypothesis: true location is greater than 0
## 95 percent confidence interval:
##  2.15  Inf
## sample estimates:
## (pseudo)median 
##            6.6
```

```r
r <- rank(abs(d)); s <- sign(d)
sum(r[s>0])
```

```
## [1] 33
```

