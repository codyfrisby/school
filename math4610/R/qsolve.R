## write a function to solve a quadratic given 3 points. 

# check for dependent package
if (!("pracma" %in% rownames(installed.packages()))) {
    install.packages("pracma")
}

qsolve <- function(x, y) { # x, y = 3 element vector
  if (length(x) > 5) stop("This function is for low order polynomials")
  x2 <- x^2
  x1 <- x
  x0 <- rep(1, length(x))
  A <- matrix(c(x2, x1, x0), ncol = length(x))
  r <- pracma::rref(cbind(A, y))
  b <- r[,dim(r)[2]]
  f <- function(x) {
   b[1] * x^2 + b[2] * x + b[3]
  }
  message("The coefs (a, b, c) of \n f(x) = ax^2 + bx + c")
  print(b)
  xmin <- min(x) - 2
  xmax <- max(x) + 2
  ymin <- min(y) - 2
  ymax <- max(y) + 2
  plot(x, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       type = "n", xlab = "x", ylab = "f(x)")
  grid()
  abline(h = 0, lty = 3) # x axis
  abline(v = 0, lty = 3)# y axis
  curve(f(x), from = xmin, to = xmax, add = TRUE, col = "red")
  points(x, y)
}

# data to test qsolve(x, y) with:
#x <- c(1, 2, 3)
#y <- c(1, 4, 9)


## Note, qsolve is obsolete now with interpolate working as intended.

# trying to generalize to any polynomial:
interpolate <- function(x, y) {
  n <- length(x)
  #if (n >= 17) stop("Sorry, I can't interpolate polynomials greater than order 16")
  # the above if statement was when I was using solve() and pracma::rref
  # something to add to this function:
  # be able to choose what order of ploynomial to interpolate.
  p <- 0:(n-1) # powers
  b <- outer(x, p, "^")
  r <- round(qr.solve(b, y), 4) # qr works for larger matricies :)
  #b <- r[,dim(r)[2]] # coeffs of our polynomial
  # I need to write an iterative vectorized function
  predict.interpolate <<- function(x) { # Creates our polynomial function.
    m <- outer(x, p, "^")
    v <- apply(m, 1, function(x) sum(x * r))
    return(v)
  }
  message("The coefs of \n f(x) =", paste0(" x", "^", p, " +"))
  print(r)
  ## now plot the function
  xmin <- min(x) - 2
  xmax <- max(x) + 2
  ymin <- min(y) - 2
  ymax <- max(y) + 2
  plot(x, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       type = "n", xlab = "x", ylab = "f(x)")
  grid()
  abline(h = 0, lty = 3) # x axis
  abline(v = 0, lty = 3)# y axis
  curve(predict.interpolate(x), from = xmin, to = xmax, 
        add = TRUE, col = "blue")
  points(x, y, col = "red", pch = 16)
}

# test example for interpolate(x, y):
#x <- 0:6
#y <- c(0,.8415,.9093,.1411,-.7568,-.9589,-.2794)