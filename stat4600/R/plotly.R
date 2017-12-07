?power.t.test()

pow <- function(x, mu, sd, n){
  1 - pnorm(x, mu, sd/sqrt(n))
}

curve(dnorm(x, 108, 4), 90, 120, col="blue")
cord.x <- c(106.58, seq(106.58, 120, 0.01), 120)
cord.y <- c(0, dnorm(seq(106.58, 120, 0.01), 108, 4),0)
#abline(v=106.58, lty=2, col = "skyblue")
polygon(cord.x, cord.y,col="lightblue", border = NA)
curve(dnorm(x, 100, 4), 80, 112, lty=3, add=T, col="red")
legend("top", legend = "Power", text.col = "lightblue", bty = "n")


library(plotly)

x <- c(1:100)
random_y <- rnorm(100, mean = 0)
data <- data.frame(x, random_y)

plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')

library(plotly)

trace_0 <- rnorm(100, mean = 5)
trace_1 <- rnorm(100, mean = 0)
trace_2 <- rnorm(100, mean = -5)
x <- c(1:100)

data <- data.frame(x, trace_0, trace_1, trace_2)

plot_ly(data, x = x) %>%
  add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines') %>%
  add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>%
  add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers') 

# note, ~ is an operator that tells R to look for the name after it in the 
# data frame.

p <- plot_ly(z = volcano, type = "surface")

df <- data.frame(x = rep(1:6, 6), y = c(rep(1,6), rep(2,6), rep(3,6), 
                                        rep(4,6), rep(5,6), rep(6,6)))
df$z <- rep(1/36, 36)
plot_ly(df, x = ~x, y = ~y, z = ~z)

plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_markers(color = ~cyl)

