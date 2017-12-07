# read the data in.  You'll have to change the directory to where the MPG
# Data.txt file is on your machine.
carmpg <- read.table("~/Documents/MATH3710/cars/MPGData.txt", header = TRUE)
summary(carmpg)
cor(carmpg)
# scatter plot.
plot(MPG ~ Weight, data=carmpg, bg = "lightblue", 
     col = "black", cex = 1.5, pch = 21)
# add the correlation to the plot.
text(x=1800, y=15, paste0("Cor = ",round(cor(carmpg)[2], 4), cex = 0.7))

# fit a linear model
fit <- lm(MPG ~ Weight, data = carmpg)
summary(fit) # displays a summary of the model.

# store the resids in a var
e <- rstandard(fit)
summary(e)
# plots of residuals
qqnorm(e)
qqline(e)
# Residuals vs. x
x <- carmpg$Weight
plot(x, e, bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
# histogram
hist(e, breaks = 25, col = "green", freq = FALSE, 
     xlim = c(-4,4), main = "Histogram of Standardized Residuals", 
     xlab = "rstandard")