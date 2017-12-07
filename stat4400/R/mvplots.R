#############################################################
######################                                      #           
# R example code to display / visualize multivariate data:  #
######################                                      #
#############################################################

# The built-in R data set called state contains a multivariate data matrix.
# For information, type:

help(state)

# To see the data matrix (containing variables for the 50 states, as of 1977):

state.x77

# Creating a data frame out of a matrix:

state.x77.df <- data.frame(state.x77)

names(state.x77.df) <- c("Popul", "Income", "Illit", "LifeExp", "Murder", "HSGrad", "Frost", "Area")

attach(state.x77.df)

############################################
############################################

# A simple scatterplot of Life Expectancy against Income:

plot(Income, LifeExp)

# A simple scatterplot of HS Graduation Rate against Income:

plot(Income, HSGrad)

# With regression line added:

plot(Income, HSGrad)
abline(lm(HSGrad~Income), lwd=2)

# With lowess regression curve added:

plot(Income, HSGrad)
lines(lowess(Income,HSGrad), lwd=2)

# With "rug plots" added to display both marginal distributions:

plot(Income, HSGrad)
rug(jitter(Income), side=1)
rug(jitter(HSGrad), side=2)

# Labeling individual observations (the states):

st.name <- abbreviate(row.names(state.x77.df) )
plot(Income, HSGrad, type='n')
text(Income, HSGrad, labels=st.name, lwd=2)

# Even better here:  R has a built-in state.abb vector, which we can use:

plot(Income, HSGrad, type='n')
text(Income, HSGrad, labels=state.abb, lwd=2)


############################################
############################################


# Finding and plotting the "convex hull" of the data:

conv.hull <- chull(Income, HSGrad)
plot(Income, HSGrad)
polygon(Income[conv.hull], HSGrad[conv.hull], density=15, angle=30)


# The ordinary sample correlation coefficient between HS Graduation Rate and Income:

cor(Income, HSGrad)

# A "robust" sample correlation coefficient between HS Graduation Rate and Income:

cor(Income[-conv.hull], HSGrad[-conv.hull])


############################################
############################################


# Copy Everitt's chiplot function:
######################
#####
chiplot<-function(x,y,vlabs=c("X","Y"),matrix="NO") {
  n<-length(x); ind<-numeric(length=n)
  for(i in 1:n) {
    for(j in (1:n)[-i])	if(x[i]>x[j]&y[i]>y[j]) ind[i]<-ind[i]+1
  }
  ind<-ind/(n-1); ind1<-numeric(length=n)
  for(i in 1:n) {
    for(j in (1:n)[-i])	if(x[i]>x[j]) ind1[i]<-ind1[i]+1
  }
  ind1<-ind1/(n-1); ind2<-numeric(length=n)
  for(i in 1:n) {
    for(j in (1:n)[-i])	if(y[i]>y[j]) ind2[i]<-ind2[i]+1
  }
  ind2<-ind2/(n-1)
  s<-sign((ind1-0.5)*(ind2-0.5))
  chi<-(ind-ind1*ind2)/sqrt(ind1*(1-ind1)*ind2*(1-ind2))
  lambda<-4*s*pmax((ind1-0.5)^2,(ind2-0.5)^2)
  thresh<-4*(1/(n-1)-0.5)^2
  if(matrix=="NO") {
    par(mfrow=c(1,2))
    plot(x,y,xlab=vlabs[1],ylab=vlabs[2])
    plot(lambda[abs(lambda)<thresh],chi[abs(lambda)<thresh],ylim=c(-1,1),xlab="lambda",
         ylab="Chi"); abline(h=1.78/sqrt(n)); abline(h=-1.78/sqrt(n))}
  if(matrix=="YES") {
    plot(lambda[abs(lambda)<thresh],chi[abs(lambda)<thresh],ylim=c(-1,1))
    abline(h=1.78/sqrt(n)); abline(h=-1.78/sqrt(n))}
}
#####
######################

# Use it to create a chi-plot to check for independence of HS Graduation Rate and Income:

chiplot(Income, HSGrad, vlabs=c("Income", "HSGrad"))

############################################
############################################


# Copy Everitt's biweight function:
# (It will be needed to construct the robust version of the bivariate boxplot)
######################
#####
biweight<-function(a,const1=9,const2=36,err=0.0001) {
  #
  #a is data matrix with two cols.
  #const1=common tuning constant
  #const2=bivariate tuning constant
  #err=convergence criterion.
  x<-a[,1]; y<-a[,2]; n<-length(x); mx<-median(x); my<-median(y); madx<-median(abs(x-mx)); mady<-median(abs(y-my))
  if(madx != 0) { ux<-(x-mx)/(const1*madx)
  ux1<-ux[abs(ux)<1]; tx<-mx+(sum((x[abs(ux)<1]-mx)*(1-ux1*ux1)^2)/sum((1-ux1^2)^2))
  sx<- sqrt(n)*sqrt(sum((x[abs(ux)<1]-mx)^2*(1-ux1*ux1)^4))/abs(sum((1-ux1*ux1)*(1-5*ux1*ux1)))
  }
  else { tx<-mx
  sx<-sum(abs(x-mx))/n
  }
  if(mady != 0) { uy<-(y-my)/(const1*mady)
  uy1<-uy[abs(uy)<1]; ty<-my+(sum((y[abs(uy)<1]-my)*(1-uy1*uy1)^2)/sum((1-uy1^2)^2))
  sy<- sqrt(n)*sqrt(sum((y[abs(uy)<1]-my)^2*(1-uy1*uy1)^4))/abs(sum((1-uy1*uy1)*(1-5*uy1*uy1)))
  }
  else { ty<-my
  sy<-sum(abs(y-my))/n
  }
  z1<-(y-ty)/sy+(x-tx)/sx; z2<-(y-ty)/sy-(x-tx)/sx; mz1<-median(z1); mz2<-median(z2); 
  madz1<-median(abs(z1-mz1)); madz2<-median(abs(z2-mz2))
  if(madz1 != 0) { uz1<-(z1-mz1)/(const1*madz1)
  uz11<-uz1[abs(uz1)<1]; tz1<-mz1+(sum((z1[abs(uz1)<1]-mz1)*(1-uz11*uz11)^2)/sum((1-uz11^2)^2))
  sz1<- sqrt(n)*sqrt(sum((z1[abs(uz1)<1]-mz1)^2* (1-uz11*uz11)^4))/abs(sum((1-uz11*uz11)*(1-5*uz11*uz11)))
  }
  else { tz1<-mz1
  sz1<-sum(abs(z1-mz1))/n
  }
  if(mady != 0) { uz2<-(z2-mz2)/(const1*madz2)
  uz21<-uz2[abs(uz2)<1]; tz2<-mz2+(sum((z2[abs(uz2)<1]-mz2)*(1-uz21*uz21)^2)/sum((1-uz21^2)^2))
  sz2<- sqrt(n)*sqrt(sum((z2[abs(uz2)<1]-mz2)^2*(1-uz21*uz21)^4))/abs(sum((1-uz21*uz21)*(1-5*uz21*uz21)))
  }
  else { tz2<-mz2
  sz2<-sum(abs(z2-mz2))/n
  }
  esq<-((z1-tz1)/sz1)^2+((z2-tz2)/sz2)^2; w<-numeric(length=n); c2<-const2
  for(i in 1:10) {
    w[esq<const2]<-(1-esq[esq<const2]/const2)^2; w[esq>=const2]<-0; l<-length(w[w==0])
    if(l<0.5*n) break
    else const2<-2*const2
  }
  tx<-sum(w*x)/sum(w); sx<-sqrt(sum(w*(x-tx)^2)/sum(w)); ty<-sum(w*y)/sum(w); 
  sy<-sqrt(sum(w*(y-ty)^2)/sum(w)); r<-sum(w*(x-tx)*(y-ty))/(sx*sy*sum(w)); const2<-c2; wold<-w
  for(i in 1:100) {
    z1<-((y-ty)/sy+(x-tx)/sx)/sqrt(2*(1+r)); z2<-((y-ty)/sy-(x-tx)/sx)/sqrt(2*(1+r)); esq<-z1*z1+z2*z2
    for(j in 1:10) {
      w[esq<const2]<-(1-esq[esq<const2]/const2)^2; w[esq>=const2]<-0; l<-length(w[w==0])
      if(l<0.5*n) break
      else const2<-2*const2
    }
    tx<-sum(w*x)/sum(w); sx<-sqrt(sum(w*(x-tx)^2)/sum(w)); ty<-sum(w*y)/sum(w); sy<-sqrt(sum(w*(y-ty)^2)/sum(w))
    r<-sum(w*(x-tx)*(y-ty))/(sx*sy*sum(w)); term<-sum((w-wold)^2)/(sum(w)/n)^2
    if(term<-err) break
    else {wold<-w
    const2<-c2
    }
  }
  param<-c(tx,ty,sx,sy,r)
  param
}
#####
######################


# Copy Everitt's bivbox function:
######################
#####
bivbox<-function(a, d = 7, mtitle = "Bivariate Boxplot",
                 method = "robust",xlab="X",ylab="Y")
{
  #a is data matrix
  #d is constant(usually 7)
  p <- length(a[1,  ])
  if(method == "robust") {
    param <- biweight(a[, 1:2]); m1 <- param[1]; m2 <- param[2]
    s1 <- param[3]; s2 <- param[4]; r <- param[5]
  }
  else {
    m1 <- mean(a[, 1]); m2 <- mean(a[, 2]); 
    s1 <- sqrt(var(a[, 1])); s2 <- sqrt(var(a[, 2])); r <- cor(a[, 1:2])[1, 2]
  }
  x <- (a[, 1] - m1)/s1; y <- (a[, 2] - m2)/s2
  e <- sqrt((x * x + y * y - 2 * r * x * y)/(1 - r * r))
  e2 <- e * e; em <- median(e); emax <- max(e[e2 < d * em * em])
  r1 <- em * sqrt((1 + r)/2); r2 <- em * sqrt((1 - r)/2); theta <- ((2 * pi)/360) * seq(0, 360, 3)
  xp <- m1 + (r1 * cos(theta) + r2 * sin(theta)) * s1; yp <- m2 + (r1 * cos(theta) - r2 * sin(theta)) * s2
  r1 <- emax * sqrt((1 + r)/2); r2 <- emax * sqrt((1 - r)/2); theta <- ((2 * pi)/360) * seq(0, 360, 3)
  xpp <- m1 + (r1 * cos(theta) + r2 * sin(theta)) * s1; ypp <- m2 + (r1 * cos(theta) - r2 * sin(theta)) * s2
  maxxl <- max(xpp); minxl <- min(xpp); maxyl <- max(ypp); minyl <- min(ypp)
  b1 <- (r * s2)/s1; a1 <- m2 - b1 * m1; y1 <- a1 + b1 * minxl; y2 <- a1 + b1 * maxxl
  b2 <- (r * s1)/s2; a2 <- m1 - b2 * m2; x1 <- a2 + b2 * minyl; x2 <- a2 + b2 * maxyl
  maxx <- max(c(a[, 1], xp, xpp, x1, x2)); minx <- min(c(a[, 1], xp, xpp, x1, x2))
  maxy <- max(c(a[, 2], yp, ypp, y1, y2)); miny <- min(c(a[, 2], yp, ypp, y1, y2))
  plot(a[, 1], a[, 2], xlim = c(minx, maxx), ylim = c(miny, maxy), xlab =xlab, ylab =ylab,
       lwd = 2, pch = 1)
  lines(xp, yp, lwd = 2); lines(xpp, ypp, lty = 2, lwd = 2)
  segments(minxl, y1, maxxl, y2, lty = 3, lwd = 2); segments(x1, minyl, x2, maxyl, lty = 4, lwd = 2)
}
#####
######################

# Use it to create a bivariate boxplot of HS Graduation Rate and Income:
# The regular method:

bivbox(cbind(Income, HSGrad), xlab = "Income", ylab = "HSGrad", method ="O")

# The robust method (which is the default):

bivbox(cbind(Income, HSGrad), xlab = "Income", ylab = "HSGrad", method ="robust")

############################################
############################################

# Copy Everitt's bivden function:
######################
#####
bivden<-function(x, y, ngridx = 30, ngridy = 30, constant.x = 1, constant.y = 1) {
  #x and y are vectors containing the bivariate data
  #ngridx and ngridy are the number of points in the grid
  mx <- mean(x); sdx <- sqrt(var(x)); my <- mean(y); sdy <- sqrt(var(y))
  #scale x and y before estimation
  x <- scale(x); y <- scale(y); den <- matrix(0, ngridx, ngridy)
  #find possible value for bandwidth
  n <- length(x); hx <- constant.x * n^(-0.2); hy <- constant.y * n^(-0.2)
  h <- hx * hy; hsqrt <- sqrt(h)
  seqx <- seq(range(x)[1], range(x)[2], length = ngridx); seqy <- seq(range(y)[1], range(y)[2], length = ngridy)
  for(i in 1:n) {
    X <- x[i]; Y <- y[i]; xx <- (seqx - X)/hsqrt; yy <- (seqy - Y)/hsqrt
    den <- den + outer(xx, yy, function(x, y)
      exp(-0.5 * (x^2 + y^2)))
  }
  den <- den/(n * 2 * pi * h); seqx <- sdx * seqx + mx; seqy <- sdy * seqy + my
  result <- list(seqx = seqx, seqy = seqy, den = den)
  result
}
#####
######################

# Use it to create a bivariate density of HS Graduation Rate and Income,
# and then plot it with the persp function:

den1 <- bivden(Income, HSGrad)
persp(den1$seqx, den1$seqy, den1$den, xlab="Income", ylab="HSGrad", zlab="Density", lwd=2, ticktype="detailed", theta=35)

# Try different values of theta (between 0 and 360) to get the best perspective.


# A contour plot of the bivariate density plotted on top of the ordinary scatterplot:

plot(Income, HSGrad)
contour(den1$seqx, den1$seqy, den1$den, lwd=2, nlevels=15, add=T)

# You can show more or fewer contours by varying the value of nlevels in this code.

############################################
############################################


# Producing a bubble plot of Income, HS Graduation Rate, and Murder Rate
# (Here Murder Rate is represented by the size of the bubbles)

plot(Income, HSGrad, pch=1, lwd=2, ylim=c(35,70), xlim=c(3000,6400))
# The x limits and y limits should be adjusted so that all the bubbles fit within the plotting window
symbols(Income, HSGrad, circles=Murder, inches=0.4, add=T, lwd=2)


# Producing a bubble plot of Income, HS Graduation Rate, and Illiteracy Rate
# (Here Illiteracy Rate is represented by the size of the bubbles)

plot(Income, HSGrad, pch=1, lwd=2, ylim=c(35,70), xlim=c(3000,6400))
# The x limits and y limits should be adjusted so that all the bubbles fit within the plotting window
symbols(Income, HSGrad, circles=Illit, inches=0.4, add=T, lwd=2)

# What if we deleted the Income-HSGrad "outliers"?

plot(Income[-conv.hull], HSGrad[-conv.hull], pch=1, lwd=2, ylim=c(35,70), xlim=c(3000,6400))
# The x limits and y limits should be adjusted so that all the bubbles fit within the plotting window
symbols(Income[-conv.hull], HSGrad[-conv.hull], circles=Illit[-conv.hull], inches=0.4, add=T, lwd=2)


############################################
############################################

# Producing a scatterplot matrix of all variables in this data set:

pairs(state.x77.df)

# Which pair of variables has the strongest positive association?
# Which pair of variables has the strongest negative association?

# Why are the scatterplots involving "Area" somewhat useless-looking here?

# With regression lines overlain on each plot:

pairs(state.x77.df, panel=function(x,y) {abline(lm(y~x),lwd=2)
  points(x,y)})

# Maximize the R graphics window to get a better presentation of these plots.

############################################
############################################


# Producing a 3-D scatterplot of Income, HS Graduation Rate, and Murder Rate

library(lattice)  # loading the lattice package

cloud(Murder ~ Income * HSGrad, xlim=range(Income), ylim=range(HSGrad), zlim=range(Murder), 
      scales = list(distance = rep(1, 3), arrows = FALSE))

# 3-D scatterplot of Income, HS Graduation Rate, and Murder Rate with "drop lines":

cloud(Murder ~ Income * HSGrad, xlim=range(Income), ylim=range(HSGrad), zlim=range(Murder), type="h",
      scales = list(distance = rep(1, 3), arrows = FALSE))

############################################
############################################

# A Conditioning Plot of HS Graduation Rate against Income, Separately for each Region:
# Note that state.region is a categorical vector in this built-in R data set

coplot(HSGrad ~ Income | state.region)

# I like the xyplot function (in the lattice package) better than the coplot function:

windows() # Opens another graphics window while still keeping the old graph up

library(lattice)
xyplot(HSGrad ~ Income | state.region)


############################################
############################################


# Creating a star plot to graphically present all variables of the state data set:

stars(state.x77.df)

# The first variable is denoted by the edge at the 3 o'clock (rightmost) position,
# and the other variables go counterclockwise around the star.

# Note how the states of Washington and Oregon resemble each other...

############################################
############################################

# Creating Chernoff Faces to graphically present all variables of the state data set:

library(TeachingDemos)
# May need to install the TeachingDemos package first?
# If so, type at the command line:  install.packages("TeachingDemos", dependencies=T)
# while plugged in to the internet.

faces( as.matrix(state.x77.df), fill=T)

# For this function, the variables correspond to facial features in this order:

# 1-height of face, 2-width of face, 3-shape of face, 4-height of mouth, 5-width of mouth, 
# 6-curve of smile, 7-height of eyes, 8-width of eyes, 9-height of hair, 10-width of hair, 
# 11-styling of hair, 12-height of nose, 13-width of nose, 14-width of ears, 15-height of ears. 

# Here's a way to generate a "key":

faces.features <- c("Face height", "Face width", "Face shape", "Mouth height", "Mouth width", 
                    "Smile curve", "Eyes height", "Eyes width", "Hair height", "Hair width", 
                    "Hair styling", "Nose height", "Nose width", "Ears width", "Ears height")

cbind( names(state.x77.df), faces.features[1:length( names(state.x77.df) )])

# To switch which columns correspond to which features, just reorder the columns in the data matrix:

faces( as.matrix(state.x77.df[c(8,6,5,2,4,1,3,7)]), fill=T)
cbind( names(state.x77.df)[c(8,6,5,2,4,1,3,7)], faces.features[1:length( names(state.x77.df) )])

# Now the 8th column (Area) corresponds to "Face height", etc.

# Note Alaska has the tallest face and the "frowniest" smile here:  Why?

# Or a similar function:

faces2(as.matrix(state.x77.df), scale="center")

# For this function, the variables correspond to facial features in this order:

# 1 Width of center 2 Top vs. Bottom width (height of split) 3 Height of Face 4 Width of top half of face 
# 5 Width of bottom half of face 6 Length of Nose 7 Height of Mouth 8 Curvature of Mouth (abs < 9) 
# 9 Width of Mouth 10 Height of Eyes 11 Distance between Eyes (.5-.9) 12 Angle of Eyes/Eyebrows 
# 13 Circle/Ellipse of Eyes 14 Size of Eyes 15 Position Left/Right of Eyeballs/Eyebrows 
# 16 Height of Eyebrows 17 Angle of Eyebrows 18 Width of Eyebrows 
