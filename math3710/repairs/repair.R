#This is an R script for the computer repair data from MATH 3710
# spring 2016 semester.  There is also an RMD file but this script
# is an attempt to use only base R so no add on packages are required.

units <- c(1,2,3,4,4,5,6,6,7,8,9,9,10,10,3)
minutes <- c(23,29,49,64,74,87,96,97,109,119,149,145,154,166, NA)
repair <- as.data.frame(cbind(units, minutes))
repair <- repair[complete.cases(repair), ] 
# we now have a data frame with two variables.
# to see a summary of the data:
summary(repair)
#correlation for units and minutes:
cor(repair, method = "pearson", use = "complete.obs")
# to see a scatter plot of the data:
plot(minutes ~ units, data=repair, bg = "lightblue", 
     col = "black", cex = 1.5, pch = 21)
# bg, col, cex, pch are all arguments to the plot function. They only determine
# how the plot appears. 

# next we fit a linear model.  we store it in an R object called m
m <- lm(minutes ~ units, data = repair)
anova(m) #ANOVA table of our model m
summary(m) #summary of linear model

# add confidnece intervals
p <- predict.lm(m, se.fit = T, interval = "confidence")
psub <- p$fit[,2:3]
colnames(psub) <- c("95% Conf Lower", "95% Conf Upper")
s <- cbind(psub, stderror = p$se.fit)
# still need to add prediction intervals
# cbind is the commmand that binds columns together in either a 
# data frame or matrix.
t <- cbind(repair$minutes, fitted.values(m), residuals(m))
colnames(t) <- c("observed", "predicted", "residuals")
cbind(t, s)

# Next we will see some of our models plots.
plot(m, which = c(1,2))
# the which argument in the plot function chooses the first two plots from the m model.
# the plot(m) call produces at least four plots.  We are only interested in the first 
# two at the moment.  You will have to hit enter in the console to view these two
# plots.

# to look at a histogram of the standardized residuals from our model.
x <- rstandard(m)
hist(x, col = "green", freq = FALSE, xlim = c(-3,3), main = "Histogram of 
     Standardized Residuals", xlab = "rstandard")
curve(dnorm(x, mean(x), sd(x)), col = 2, add = TRUE, 
      from = quantile(x, 0.025), to = quantile(x, 0.975))
