plot(6:25,rnorm(20),type="b",xlim=c(1,30),ylim=c(-2.5,2.5),col=2)
par(new=T)
plot(rnorm(30),type="b",axes=F,col=3)
par(new=F)


par(mfrow=c(1,2))
fit2 <- lm(log(Carbon) ~ log(City), data=fuel)
plot(jitter(Carbon) ~ jitter(City), xlab="City (mpg)",
     ylab="Carbon footprint (tonnes per year)", data=fuel)
lines(1:50, exp(fit2$coef[1]+fit2$coef[2]*log(1:50)))
plot(log(jitter(Carbon)) ~ log(jitter(City)), 
     xlab="log City mpg", ylab="log carbon footprint", data=fuel)
abline(fit2)

par(mfrow=c(1,2))
time <- 1:15
num <- c(355,211,197,166,142,106,104,60,56,38,36,32,21,19,15)
D <- as.data.frame(cbind(time, num))

fit2 <- lm(log(num) ~ time, data = D)
plot(jitter(num) ~ jitter(time), xlab="time", ylab="number of bacteria", data=D)
lines(1:50, exp(fit2$coef[1]+fit2$coef[2]*1:50))
plot(log(num) ~ time, data = D, pch = 16)
abline(fit2)
scatterplot(num ~ time)

# draw a smooth line through a scatter plot
plot(cars, main = "Stopping Distance versus Speed")
lines(stats::lowess(cars))
plot(D)
lines(lowess(D), col = 2)
lines(lowess(D, f = 0.2), col = 3)
lines(lowess(D, f = 0.5), col = 4)
lines(lowess(D, f = 1), col = 5)

plot(cars, main = "lowess(cars)")
lines(lowess(cars), col = 2)
lines(lowess(cars, f = .2), col = 3)
legend(5, 120, c(paste("f = ", c("2/3", ".2"))), lty = 1, col = 2:3)
