# runge2.r          Observe Runge phenomenon with interpolation

npts = 100                       # points for plotting
x <- 4.1 * (2 * (1:npts) - npts - 1)/ (npts - 1) # abscissas for plot
s <- 1/( 1 + x^2)                   # true function
# now interpolate for both n=7 and n=9 points

n <- 7                           # how many points for interpolation?
xi <- 4 * cos((2 * (1:n) - 1) * pi / (2 * n)) # Chebyshev interpolation pts    
b <- 1 / (1 + xi^2)                 # function values
A <- outer(xi,(0:(n-1)),FUN="^") # vandermonde matrix
b7 <- solve(A, b)                 # solution for 7 points
y7 <- outer(x, (0:(n-1)), FUN="^") %*% b7 # interpolated values

n <- 9                           # how many points for interpolation?
xi <- 4 * cos((2 * (1:n) - 1) * pi / (2 * n)) # Chebyshev interpolation pts    
b <- 1 / (1 + xi^2)              # function values
A <- outer(xi, (0:(n-1)),FUN="^")# vandermonde matrix
b9 <- solve(A,b)                 # solution for 7 points
y9 <- outer(x,(0:(n-1)),FUN="^") %*% b9 # interpolated values
# now get ready to plot
#print(cbind(x,s,y7,y9))         # if we want to see it all
plot(x, s, type = "l", xlab = "x", ylab = "z(x)", ylim = c(-0.2,1)) # Figure 7.2
grid(); abline(v = 0, lty = 3); abline(h = 0, lty = 3)
lines(x, y7, lty=3, lwd = 2, col = "dodgerblue")
lines(x, y9, lty=3, lwd = 2, col = "red")