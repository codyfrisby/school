########  using to test some code:
df <- read.csv("~/Documents/STAT4400/dat/USairpollution.csv")
id <- df$X
df <- df[-1]
combos <- t(combn(1:length(names(df)), 2))

rand <- sample(1:dim(combos)[1], 1)
a <- df[, combos[rand, ]]

####  bvplot function start
  d <- 7
  param <- biweight(a[, 1:2])
  m1 <- param[1]
  m2 <- param[2]
  s1 <- param[3]
  s2 <- param[4]
  r <- param[5]
  x <- (a[, 1] - m1)/s1
  y <- (a[, 2] - m2)/s2
  e <- sqrt((x * x + y * y - 2 * r * x * y)/(1 - r * r))
  e2 <- e * e
  em <- median(e)
  emax <- max(e[e2 < d * em * em])
  r1 <- emax * sqrt((1 + r)/2)
  r2 <- emax * sqrt((1 - r)/2)
  theta <- ((2 * pi)/360) * seq(0, 360, 3)
  xpp <- m1 + (r1 * cos(theta) + r2 * sin(theta)) * s1
  ypp <- m2 + (r1 * cos(theta) - r2 * sin(theta)) * s2
  maxxl <- max(xpp)
  minxl <- min(xpp)
  maxyl <- max(ypp)
  minyl <- min(ypp)
  maxx <- max(c(a[, 1], xpp))
  minx <- min(c(a[, 1], xpp))
  maxy <- max(c(a[, 2], ypp))
  miny <- min(c(a[, 2], ypp))
  plot(a[, 1], a[, 2], xlim = c(minx - m1, maxx), 
         ylim = c(miny - m2, maxy))
  lines(xpp, ypp, lty = 2)
  ##### bvplot function end....
  sx <- a[,1] - m1; sy <- a[,2] - m2
  points(sx, sy)
  lines(xpp - m1, ypp - m2, lty = 2)
  A <- cov(cbind(sx, sy))
  ##########  code from http://stats.stackexchange.com/questions/9898/how-to-plot-an-ellipse-from-eigenvalues-and-eigenvectors-in-r
  ctr <- c(m1, m2) # data centroid -> colMeans(dataMatrix)
  RR <- chol(A) # Cholesky decomposition
  angles <- seq(0, 2*pi, length.out = 200) # angles for ellipse
  ell <- 1 * cbind(cos(angles), sin(angles)) %*% RR  # ellipse scaled with factor 1
  ellCtr <- sweep(ell, 2, ctr, "+") # center ellipse to the data centroid
  lines(ellCtr, lwd=1, asp=1, lty = 3, col = "blue") # plot ellipse
  points(ctr[1], ctr[2], pch=4, lwd=2) # plot data centroid
  
  eigVal <- eigen(A)$values
  eigVec <- eigen(A)$vectors
  eigScl <- eigVec %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
  xMat <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])  
  yMat <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
  ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) 
  # normal ellipse
  ellRot <- eigVec %*% t(ellBase)                                          
  # rotated ellipse
  lines((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], asp=1, type="l", lwd=1,
        col = "red")
  lines(ellBase)
  matlines(xMat, yMat, lty=1, lwd=1, col="blue")
  points(ctr[1], ctr[2], pch=4, col="red", lwd=3)
  ######################################################
  
  sxpp <- (xpp - m1) / s1 # unscale and center
  sypp <- (ypp - m2) / s2 # unscale and center
# center is now the origin.
  s <- diag(c(r1, r2))
  rx <- cos(theta) + sin(theta)
  ry <- cos(theta) - sin(theta)
# not equal
  temp <- t(s %*% t(cbind(rx, ry))) # attempt to draw the same ellipse
  # only centered at the origin and NOT rotated.
  
print(round(t(newx[,1]) %*% newx[,2], 8)) # these two vectors are orthoganal.

  maa <- sqrt(sum(newx[,1]^2))/2 # half length of major or minor axis
  mia <- sqrt(sum(newx[,2]^2))/2 # half length of minor or major axis
# how many points are outside the ellipse?
print(sum((x^2/ maa ^ 2) + (y^2 / mia ^ 2) > 1))


#### plots for debugging 
plot(x, y, xlim = c(min(x, sxpp) - 1, max(x, sxpp) + 1), 
     ylim = c(min(y, sypp) - 1, max(y, sypp) + 1))
lines(sxpp, sypp, lty = 3)
points(0, 0, pch = 16)
lines(temp)
arrows(0,0,r1,r2)

# and they draw an ellipse that is NOT rotated and centered at the origin

# are these now the foci?????
points(-r1, 0, pch = 16)
points(0, r2, pch = 16)
points(r1, 0, pch = 16)
points(0, -r2, pch = 16)

points(r1 * cos(theta), r2 * sin(theta))
points(r1 * cos(theta) + r2 * sin(theta), 
       r1 * cos(theta) - r2 * sin(theta))
points(r1 * (cos(theta) + sin(theta)), 
       r2 * (cos(theta) - sin(theta)))

r90 <- matrix(c(cos(theta[30]), sin(theta[30]), 
                -sin(theta[30]), cos(theta[30])), ncol = 2)
ar <- diag(diag(c(r1, r2)) %*% r90)
arrows(0, 0, ar[1], ar[2])
