#! usr/local/bin/Rscript
# Functions I'm writing for math 4610, Numerical Analysis class.  
# These are root finding algorithms.

#f <- function(x) x^2 - 2

####################### Bisection method ##################
bisection <- function(a, b, FUN = f, tol = .Machine$double.eps^0.5, num = 20){
  if (FUN(0) == 0) stop("Zero is a root.")
  if (FUN(a) * FUN(b) > 0) stop("f(a) and f(b) can not be the same sign.")
  i <- 1
  Fa <- FUN(a)
  Fp <- FUN((a + b) / 2) # need for control
  guess <- (a + b) / 2 # if Fp is the correct answer to begin with
  a1 <- a
  b1 <- b
  iteration <- i
  while (abs(Fp) > tol) {
    p <- (a + b) / 2 # middle of a and b
    Fp <- FUN(p)
    i <- i + 1
    a1[i] <- a
    b1[i] <- b
    guess[i] <- p # store the result in a vector
    iteration[i] <- i
    if (Fa * Fp > 0) { # bisection method test
      a = p # a moves if Fa and Fp are the same sign
      Fa = Fp
    } else {
      b = p # b moves if Fa and Fp are opposite signs
    }
    if (i == num){
      stop("Error > tol.  Max iterations reached")
    }
  }
  RESULT <- cbind(guess, a1, b1, iteration) # for printing/storing purposes
  colnames(RESULT) <- c("guess", "a", "b", "iteration") # meaningful name
  return(RESULT)
  #return(sprintf("[%f, %f], %f", a1, b1, guess))
  # C printing : printf("[%f,%f], %f \n", left,right,middle);
}


f.plot <- function(x = 1:10, FUN = f, xlim = c(-5, 5), ylim = c(-5, 5), 
                   from = -5, to = 5) { # plot our function
  plot(x, xlim = xlim, ylim = ylim, type = "n", xlab = "", ylab = "")
  grid()
  curve(FUN(x), from, to, add = TRUE, col = "red")
  abline(h = 0, lty = 3) # x axis
  abline(v = 0, lty = 3)# y axis
  abline(coef = c(0, 1), lty = 3, col = "dodgerblue")
}

# diverges
#g <- function(x) { # function for testing fixed point method
#  x^3 + 4 * x^2 - 10
#}
# rewrite it and we have convergence
#g <- function(x) {
#  0.5 * sqrt((10 - x^3))
#}

############# Fixed Point Method ###########################
fixedpoint <- function(p0 = 1, FUN = g, tol = 1e-6, n = 100) {
  i = 1
  guess <- p0
  while (i < n) {
    p = FUN(p0)
    if(is.infinite(p)) stop("Algorithm is diverging. Pick a different starting point or rewrite g(x)")
    if(abs(p - p0) < tol) {
      guess[i] <- p
      return(cbind(guess))
      break
    } else {
      i = i + 1
      if (i == n) stop("Failed to converge")
      p0 = p
      #if (p0 > 1e6) stop("p0 is diverging!  Consider rewriting g(x).")
      guess[i] <- p0
    }
  }
  RESULT <- cbind(guess)
  return(RESULT)
}
## fixed point method with plotting
fixed_plot <- function(x, FUN = cos, n = 10) {
  xmin <- -abs(x)
  xmax <- abs(x)
  plot(1:abs(x), xlim = c(xmin - 1, xmax + 1), 
       ylim = c(-2, 2), type = "n", xlab = "", ylab = "")
  grid()
  abline(h = 0, lty = 3) # x axis
  abline(v = 0, lty = 3)# y axis
  abline(coef = c(0, 1), col = "dodgerblue", lty = 3)
  curve(FUN(x), from = xmin - 2, xmax + 2, lwd = 2, add = TRUE)
  x0 <- x
  i <- 1
  m <- matrix(ncol = 3)
  for(i in 1:n) {
    x0 <- FUN(x)# calculate g(x)
    e <- abs(x0 - x)
    points(x, x0, col = i)
    m <- rbind(m, c(x, x0, e))
    x <- x0
    #print(x); print(r); print(a)
    #Sys.sleep(0.1)
  }
  points(x0, FUN(x0), pch = "X", cex = 2)
  m <- m[-1, ] # remove the stupid NA row
  colnames(m) <- c("x", "x0", "diff")
  return(m)
}
################ Newton's Method ##########################
# newton's method needs two function: f and f'
#f <- function(x) cos(x) - x
#fprime <- function(x) -sin(x) - 1 

newton2 <- function(x0 = 0, FUN = f, FP = fp, 
                   tol = .Machine$double.eps^0.5, n = 100) {
  i <- 1
  guess <- numeric()
  fx <- numeric()
  dfx <- numeric()
  error <- numeric()
  e <- tol + 1
  while (e >= tol && i <= n) {
    fx1 <- FUN(x0)
    dfx1 <- FP(x0)
    guess[i] <- x0  # store ith iteration 
    fx[i] <- fx1
    dfx[i] <- dfx1 # store ith iteration
    error[i] <- abs(-fx1/dfx1) # store ith iteration
    x <- x0 - fx1 / dfx1 # calculate x
    if (is.infinite(x - x0)) stop("dx = 0.  Can't divide by 0. Pick a different x0")
    if (abs(x - x0) < tol) {
      return(cbind(guess, fx, dfx, error))
      break
    } else {
      i <- i + 1
      x0 <- x # update for next loop
    }
    #if (i == n) stop("Max number of iterations reached.")
  }
  return(cbind(guess, fx, dfx, error))
}

############# Secant Method ###############################
secant2 <- function(FUN, p0, p1, n = 20, tol = .Machine$double.eps^0.5) {
  i <- 1
  q0 <- FUN(p0)
  q1 <- FUN(p1)
  guess <- numeric()
  iteration <- numeric()
  e <- numeric()
  while (i <= n) {
    r <- p1 - q1 * (p1 - p0)/(q1 - q0)
    e[i] <- abs(1 - abs(r)/abs(p1))
    guess[i] <- r
    iteration[i] <- i
    if (abs(r - p1) < tol) { # we found the root
      return(cbind(guess, iteration, e))
      break
    } else {
      # update everything
      i <- i + 1
      p0 <- p1
      q0 <- q1
      p1 <- r
      q1 <- FUN(r)
    }
  }
  return(cbind(guess, iteration, e))
}
###########################################################
########## False Position Method ##########################
Fposition <- function(FUN = f, p0, p1, tol = .Machine$double.eps^0.5, n = 20) {
  i <- 1
  q0 <- FUN(p0)
  q1 <- FUN(p1)
  guess <- numeric()
  iteration <- numeric()
  a <- numeric() # a and b are our brackets.  Root should be within [a,b]
  b <- numeric()
  while (i <= n) {
    p <- p1 - q1 * (p1 - p0)/(q1 - q0)
    guess[i] <- p
    iteration[i] <- i
    a[i] <- p0
    b[i] <- p1
    if (abs(p - p1) < tol) { # we found the root
      return(cbind(guess, a, b, iteration))
      break
    } else {
      # update everything
      i <- i + 1
      q <- FUN(p)
      if (q * q1 < 0) {
        p0 <- p1
        q0 <- q1
      }
      p1 <- p
      q1 <- q
    }
  }
  return(cbind(guess, a, b, iteration))
}
###########################################################
################ Cubic Convergence ########################
# newton's method needs two function: f and f'
#f <- function(x) cos(x) - x
#fp <- function(x)  -sin(x) - 1

cubic <- function(x0 = 0, FUN = f, FP = NULL,
                   tol = .Machine$double.eps^0.5, n = 100) {
  if(is.null(FP)) {
    FP <- function(x, ...) {
      h <- tol^(2/3)
      (FUN(x + h, ...) - FUN(x - h, ...))/(2 * h)
    } 
  }
  sx <- function(x, ...) {
    h <- tol^(2/3)
    (FP(x + h, ...) - FP(x - h, ...))/(2 * h)
  }
  i <- 1
  guess <- numeric()
  error <- numeric()
  x <- x0
  while (i <= n) {
    fx <- FUN(x0)
    dfx <- FP(x0)
    sx1 <- sx(x0)
    guess[i] <- x0  # store ith iteration 
    x <- x0 - (1/dfx) * fx - (1/2) * (fx^2 * sx1)/(dfx^3) # add another "term"?
    error[i] <- abs(x - x0) # store ith iteration
    if (abs(x - x0) < tol) {
      return(cbind(guess, error))
      break
    } else {
      i <- i + 1
      x0 <- x # update for next loop
    }
    #if (i == n) stop("Max number of iterations reached.")
  }
  return(cbind(guess, error))
}

iterate_plot <- function(x, FUN = cos, ..., n = 10, plot = TRUE) {
  x0 <- x
  i <- 1
  m <- matrix(ncol = 3)
  for(i in 1:n) {
    x0 <- FUN(x, ...)# calculate g(x)
    e <- (x0 + x) / 2
    m <- rbind(m, c(x, x0, e))
    x <- x0
    #print(x); print(r); print(a)
    #Sys.sleep(0.1)
  }
  if (plot){
    xmin <- -abs(x)
    xmax <- abs(x)
    plot(1:abs(x), xlim = c(xmin - 1, xmax + 1), 
         ylim = c(-2, 2), type = "n", xlab = "", ylab = "")
    grid()
    abline(h = 0, lty = 3) # x axis
    abline(v = 0, lty = 3)# y axis
    curve(FUN(x, ...), from = xmin - 2, xmax + 2, lwd = 2, add = TRUE)
  }
  m <- m[-1, ] # remove the stupid NA row
  colnames(m) <- c("x", "x0", "ave")
  return(m)
}

### write a function that finds which error
f <- function(x) x^2 - 3 - 0.001 # function for example
fp <- function(x) 2 * x # derivative
# use newton's method to find x0 for the 3rd iterate
## start: 
#p0 <- 1
#p1 <- 4

err <- function(p0 = 1, p1 = 4, n = 30, e = 0.001) {
  q0 <- newton2(x0 = p0, n = 3)[3, 4] - e
  q1 <- newton2(x0 = p1, n = 3)[3, 4] - e
  r <- p1 - q1 * ((p1 - p0) / (q1 - q0))
  for(i in 1:n) {
    q0 <- newton2(x0 = p0, n = 3)[3, 4] - e
    q1 <- newton2(x0 = p1, n = 3)[3, 4] - e
    r <- p1 - q1 * ((p1 - p0) / (q1 - q0))
    if (abs(q1) <= 1e-14) {
      break
    }
    i <- i + 1
    p0 <- p1
    q0 <- q1
    p1 <- r
    q1 <- newton2(x0 = r)[3, 4] - 0.001
  }
  names(r) <- NULL
  CHECK <- newton2(x0 = r, n = 3)
  RESULT <- r
  return(list(x0 = r, check = CHECK))
}

# check the results:
#newton2(x0 = 2.251616, n = 3) 

###################### Horner's Method (2 versions)########
horner2 <- function(x, v) { # Horner's method
  Reduce(v, right = TRUE, f = function(a, b) b * x + a)
}

horner1 <- function(a, x) { # a are the coeffs from 0 to n
  y <- 0
  b <- vector()
  i <- 1
  for(c in rev(a)) {
    y <- y * x + c
    b[i] <- y
    i <- i + 1
  }
  return(list(x = y, b = b))
}

# Test example
#a <- rev(c(4, 0, -2, 3, 1)) # polynomial of degree 3
#newton3(a, -2)
#f <- function(x) 4*x^4 - 2*x^2 + 3*x + 1

newthorn <- function(a, x0, n = NULL) { # a are the coeffs from 0 to n
  b <- vector()
  if (is.null(n)) n <- length(a)
  k <- length(a)
  for(i in 1:n) {
    h <- horner1(a, x0) # the numerator of Newton's method
    d <- h$b[-k] # take one off cuz it's the solution to the horner poly
    h2 <- horner2(x0, rev(d)) # denominator of Newton's
    b[i] <- x0 - (h$x / h2) # evaluate
    x0 <- b[i] # update x0
    if(abs(h$b[k]) <= 1e-10) {
      message(paste(b[i], "is a root"))
      # Need to add multiple root finding
    }
  }
  return(list(guesses = b, lastHorn = h$b))
}

## Test case for allRealRoots() :
a <- c(-5040, 1602, 1127, -214, -72, 4, 1)
x0 <- 8
#################  ALL REAL ROOTS, FAST ###################
allRealRoots <- function(a, x0, n = 20) { # a are the coeffs from 0 to n
  b <- vector()
  roots <- vector()
  for (j in 1:(length(a)-1)) { # outer loop to grab all the real roots
    k <- length(a)
    for(i in 1:n) { # inner loop to find the jth real root
      h <- horner1(a, x0) # the numerator of Newton's method
      if(abs(h$x) <= 1e-10) { # test for root
        message(paste(x0, "is a root"))
        a <- rev(h$b)[-1] # gotta knock off a degree!
        roots[j] <- x0 # store the jth root
        break # get out of inner loop
      }
      d <- h$b[-k] # take one off cuz it's the solution to the horner poly
      h2 <- horner2(x0, rev(d)) # denominator of Newton's
      x0 <- x0 - (h$x / h2) # evaluate and update
    }
  }
  return(list(Roots = roots))
}

# check:
#f <- function(x) { x^6 + 4*x^5 - 72*x^4 - 214*x^3 + 1127*x^2 + 1602*x - 5040}
