require(rgl)
x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x, y)
plot3d(x, y, z, col = rainbow(1000))

require(rgl)
x <- plastic$temp; y <- plastic$pressure; z <- plastic$strength
plot3d(x,y,z, col = 2)


library(ggplot2)
ggplot(hospital, aes(y = y, x = x1)) +
  scale_color_discrete(x2,x3) +
#  scale_colour_gradient(limits=c(min(x2), max(x2)), 
 #                       low="green", high="red") +
  geom_point() + 
  stat_smooth()

library(ggplot2) # histograms
x <- as.data.frame(bias[,3]); names(x) <- "bias"
g <- ggplot(x, aes(x=bias))
g <- g + geom_histogram(colour = "lightblue", fill = "white", 
                        aes(y = ..density..))
g <- g + geom_density()
g

# Add a Normal Curve (Thanks to Peter Dalgaard)
x <- bias[,3]
draw.hist <- function(x) { # use this function when bootstrapping
  h <- hist(
    x, breaks = 30, col = "lightblue", xlab = "", main = ""
  ) 
  xfit <- seq(min(x),max(x), 0.01)
  yfit <- dnorm(xfit,mean = mean(x),sd = sd(x))
  yfit <- yfit * diff(h$mids[1:2]) * length(x)
  lines(xfit, yfit, col = "black", lwd = 2)
}

# ggplot qplot option
qplot(x3, y, colour=x2) + 
  scale_colour_gradient(limits=c(min(x2), max(x2)), 
                        low="green", high="red")

# GG linear plot with R^2 and equation printed on plot

# Create a list of character strings - the first component
# produces the fitted model, the second produces a
# string to compute R^2, but in plotmath syntax.
mod <- lm(life$LifeExp ~ fit.all$fitted.values)
rout <- list(paste('Fitted model: ', round(coef(mod)[1], 4), ' + ',
                   round(coef(mod)[2], 3), ' x', sep = ''),
             paste('R^2 == ', round(summary(mod)[['r.squared']], 3),
                   sep = ''))
#rout
df <- as.data.frame(cbind(x = fit.all$fitted.values, y = life$LifeExp))

fitplot <- ggplot(df, aes(x, y)) +
  geom_smooth(method = 'lm') + geom_point() + xlab("Predicted") + 
  ylab("Observed") +
# need to change these x and y values below
  geom_text(aes(x = 50, y = 80, label = rout[[1]]), hjust = 0) +
  geom_text(aes(x = 50, y = 78, label = rout[[2]]), hjust = 0, 
            parse = TRUE)

