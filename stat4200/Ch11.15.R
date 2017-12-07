#STAT 4200 Problem 11.15 6th or 7th Edition.

x <- c(6.7,8.2,7.9,6.4,8.3,7.2,6,7.4,
       8.1,9.3,8.2,6.8,7.4,7.5,8.3,9.1,8.6,7.9,6.3,8.9)
y <- c(7.1,8.4,8.2,6.9,8.4,7.9,6.5,
       7.6,8.9,9.9,9.1,7.3,7.8,8.3,8.9,9.6,8.7,8.8,7.0,9.4)
r <- sum(y)/ sum(x)
Rx <- y - r*x
#above data are from book.  Below is my code.
boot <- function(n = 100, dat, parameter){
        X <- matrix(sample(dat, n*length(dat), replace = TRUE),
                    n, length(dat))
        Bootstrap <- apply(X, 1, parameter)
        
        c(quantile(Bootstrap, c(0.025, 0.975)), mean(Bootstrap),
          mean(dat), hist(Bootstrap, col = "green", freq = FALSE),
          abline(v=mean(Bootstrap), col = "magenta", lwd = 3))

}