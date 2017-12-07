quad <- function(a, b, c) { # quadratic formula
  (-b  + c(1, -1) * sqrt(b^2 - (4 * a * c))) / (2 * a)
}

quad(18, 48, -18)

inelastic <- function(m1, m2, theta1, theta2, v1, v2, degrees = TRUE) {
  if (degrees) {
    x <- v1 * c(cos(theta1 * pi / 180), sin(theta1 * pi / 180))
    y <- v2 * c(cos(theta2 * pi / 180), sin(theta2 * pi / 180))
  } else {
    x <- c(v1 * cos(theta1), v1 * sin(theta1))
    y <- c(v2 * cos(theta2), v2 * sin(theta2))
  }
  RESULT <- (m1 * x + m2 * y) / (m1 + m2)
  return(RESULT)
}

inelastic(6, 12, 50, 165, 3, 2)
# 8 on exam 1 review:
inelastic(200, 190, 180, 0, 6, 8)

elastic <- function(m1, m2, theta1, theta2, v1, v2, degrees = TRUE) {
  if (degrees) {
    x <- v1 * c(cos(theta1 * pi / 180), sin(theta1 * pi / 180))
    y <- v2 * c(cos(theta2 * pi / 180), sin(theta2 * pi / 180))
  } else {
    x <- c(v1 * cos(theta1), v1 * sin(theta1))
    y <- c(v2 * cos(theta2), v2 * sin(theta2))
  }
  ## more stuff here
}

# more formulas from the board
#I = delta p = m * delta v
#I = f * delta t

lab05 <- function(x0 = 0, y0 = 0, theta0 = 30, degrees = TRUE, v0 = 50,
                  drag = 0.01, dt = 0.2, g = -9.8, plot = FALSE, 
                  converge = TRUE, n = NULL) {
  if (degrees) {
    vx0 <- v0 * cos(theta0 * pi/180)
    vy0 <- v0 * sin(theta0 * pi/180) 
  } else {
    vx0 <- v0 * cos(theta0)
    vy0 <- v0 * sin(theta0) 
  }
  time <- seq(0, 100, dt)
  if (is.null(n)) {
    n <- length(time)
  }
  vy <- vy0
  vx <- vx0
  x <- x0
  y <- y0
  # loop for now
  for (i in 1:n) {
    vx[i+1] <- vx[i] - (time[i+1] - time[i]) * (sqrt(vx[i]^2 + vy[i]^2) * vx[i] * drag)
    vy[i+1] <- vy[i] + (time[i+1] - time[i]) * (g - sqrt(vx[i]^2 + vy[i]^2) * vy[i] * drag)
    x[i+1] <- x[i] + (time[i+1] - time[i]) * vx[i]
    y[i+1] <- y[i] + (time[i+1] - time[i]) * vy[i]
    if(converge) {
      if (y[i+1] < 0)
        break
    }
  }
  rm_na <- !is.na(vy)
  n <- length(vy[rm_na])
  time <- time[1:n]
  RESULT <- data.frame(time, vy[rm_na], vx[rm_na], x[rm_na], y[rm_na])
  names(RESULT) <- c("time", "vy", "vx", "x", "y")
  if (plot) {
    library(ggplot2)
    gd <- ggplot(data = RESULT, aes(x = x, y = y)) + 
      geom_line(color = "dodgerblue") + ylab("Height") + xlab("Displacement")
    print(gd)
  }
  return(RESULT)
}

lab05.2 <- function(km = 200, bm = 3, x0 = 0.001, v0 = 0, m = 1, 
                    dt = 0.01, n = NULL, plot = FALSE) {
  vx <- x0
  x <- x0
  k <- km / m
  b <- bm / m
  time <- seq(0, 100, dt)
  if (is.null(n)) {
    n <- length(time)
  }
  for (i in 1:n) {
    vx[i+1] <- vx[i] + ((time[i+1] - time[i]) * (-k * x[i] - b*vx[i]))
    x[i+1] <- x[i] + (time[i+1] - time[i]) * vx[i]
  }
  rm_na <- !is.na(vx)
  n <- length(vx[rm_na])
  time <- time[1:n]
  RESULT <- data.frame(time, vx[rm_na], x[rm_na])
  names(RESULT) <- c("time", "vx", "x")
  return(RESULT)
  if (plot) {
    library(ggplot2)
    df <- reshape::melt(RESULT[, c("vx", "x")])
    df$time <- RESULT$time
    gx <- ggplot(data = df, aes(x = time, y = value, color = variable)) + 
      geom_line() + ylab("") + xlab("Time (s)") + 
      theme(legend.title = element_blank())
    print(gx)
  }
  return(RESULT)
}


lab05.3 <- function(g = 9.8, dt = 0.0005, n = NULL, vy0 = 2, 
                    x0 = 1.2, y0 = 0) {
  time <- seq(0, 100, dt)
  vx <- 0
  vy <- vy0
  x <- x0
  y <- y0
  if (is.null(n)) {
    n <- length(time)
  }
  for (i in 1:n) {
    dt <- time[i+1] - time[i]
    vx[i+1] <- vx[i] - dt * g * x[i] / (x[i]^2 + y[i]^2)^(3/2)
    vy[i+1] <- vx[i] - dt * g * y[i] / (x[i]^2 + y[i]^2)^(3/2)
    x[i+1] <- x[i] + dt * vx[i]
    y[i+1] <- y[i] + dt * vy[i]
  }
  rm_na <- !is.na(vx)
  n <- length(vx[rm_na])
  time <- time[1:n]
  RESULT <- data.frame(time, x[rm_na], y[rm_na], vx[rm_na], vy[rm_na])
  names(RESULT) <- c("time", "x", "y", "vx", "vy")
  return(RESULT)
}
