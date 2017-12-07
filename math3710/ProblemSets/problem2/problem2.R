# Problem set # 2
agebp <- read.table("MATH3710/datafiles/AGEBP.DAT")
names(agebp) <- c("BP", "Age")
x <- agebp$Age; y <- agebp$BP
ssx <- sum((x - mean(x))^2)
ssy <- sum((y - mean(y))^2)
sxy <- sum((x-mean(x))*(y-mean(y)))
xbar <- mean(x); ybar <- mean(y)
corcoef <- sxy/sqrt(ssx*ssy)
# is SLR appropriate?
cor(x,y); plot(y ~ x, pch = 16)
# yes ^
text(x=62, y=100, paste0("Cor = ", round(cor(x,y), 4)), cex = 0.7)
b1 <- sxy/ssx
b0 <- mean(y) - (b1 * mean(x))
sse <- ssy - (sxy^2/ssx)
mse <- sse/(length(x) - 2)
rmse <- sqrt(mse)