library(UsingR) # for the diamond data
fit <- lm(price ~ carat, data = diamond)
# fit with mean center
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)[1] #averge price for a one carat diamond
# scale down the predictor coef.
fit3 <- lm(price ~ I(carat * 10), data = diamond)
# test some diamond sizes
newx <- c(0.16, 0.27, 0.34)
# see what the model predicts for cost.
coef(fit)[1] + coef(fit)[2] * newx
predict(fit, newdata = data.frame(carat = newx))

# plot the data
library(ggplot2)
g <- ggplot(diamond, aes(x = carat, y = price))
g <- g + xlab("Mass (carats)")
g <- g + ylab("Price (SIN $)")
g <- g + geom_point(size = 7, colour = "black", alpha = 0.2)
g <- g + geom_point(size = 5, colour = "blue", alpha = 0.2)
g <- g + geom_smooth(method = "lm", colour = "black")
g
