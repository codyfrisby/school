f <- function(r) { # draw a circle
  theta <- seq(0, 2*pi, 0.01)
  t0 <- sample(theta, 1)
  x <- r * cos(theta)
  y <- r * sin(theta)
  xmax <- max(x) + 1; ymax <- max(y) + 1
  xmin <- min(x) - 1; ymin <- min(y) - 1
  plot(x, y, xlim = c(xmin, xmax), ylim = c(ymin, ymax),
       type = "l", col = "dodgerblue", 
       main = paste("R =", r, ",", "Theta =", round(t0 * 180/pi)))
  grid()
  abline(v = 0, lty = 3)
  abline(h = 0, lty = 3)
  arrows(0, 0, x1 = r * cos(t0), 
         y1 = r * sin(t0))
}

f(sample(1:10, 1))


