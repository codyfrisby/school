# functions used when k = 2 clusters
center <- function(y){
  if(!is.data.frame(y))
    stop("can't use this function on a single vector")
  m <- apply(y, 2, mean) # center to compare x to
  return(m)
}
# function to calculate "by hand" for k=2 means clusters
distance <- function(x, y) {
  centroid <- center(y)
  dx <- sum((x - centroid) ^ 2)
  return(list(dist_x = dx, center = centroid))
}
# data to test out functions:
#x1 <- c(5,-1,1,-3)
#x2 <- c(3,1,-2,-2)
#labs <- LETTERS[1:4]
#df <- data.frame(x1, x2)
#rownames(df) <- labs
#A <- df[1,]; B <- df[2, ]; C <- df[3, ]; D <- df[4, ]
library(ggplot2)
plot.components <- function(x, y, z, xlab = NULL, ylab = NULL) {
  dt <- data.frame(x, y, z)
  names(dt) <- c("x", "y", "z")
  gg <- ggplot(data = dt, aes(x = x, y = y)) + theme_bw()
  gg <- gg + geom_text_repel(aes(label = z), size = 1.5,
                             segment.alpha = 0.25,
                             segment.size = 0.25)
  gg <- gg + geom_point(color = "red", size = 3, alpha = 0.5)
  gg <- gg + xlab(xlab) + ylab(ylab)
  return(gg)
}

