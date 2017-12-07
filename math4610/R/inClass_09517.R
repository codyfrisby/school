test <- function(p0 = 1, C = -3/4, n = 30) {
  e <- vector()
  p <- vector()
  for (i in 1:n){
  p[i] <- p0 + C * (exp(p0) - 2)
  e[i] <- 1 - (p[i]/p0)
  p0 <- p[i]
  }
  return(cbind(p, e))
}

# visualize the convergence:
plot(test()[,1], type = "b", ylab = "Approximation", xlab = "Iteration")
grid()
abline(h = log(2), col = "red", lty = 3)
abline(h = 0, col = "red", lty = 3)
