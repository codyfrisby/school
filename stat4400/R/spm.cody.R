spm.cody <- function (x, var.labels = colnames(x), diagonal = c("density", 
                                                    "boxplot", "histogram", "oned", "qqplot", "none"), adjust = 1, 
          nclass, plot.points = TRUE, smoother = loessLine, smoother.args = list(), 
          smooth, span, spread = !by.groups, reg.line = lm, transform = FALSE, 
          family = c("bcPower", "yjPower"), ellipse = FALSE, levels = c(0.5, 
                                                                        0.95), robust = TRUE, groups = NULL, by.groups = FALSE, 
          use = c("complete.obs", "pairwise.complete.obs"), labels, 
          id.method = "mahal", id.n = 0, id.cex = 1, id.col = palette()[1], 
          id.location = "lr", col = if (n.groups == 1) palette()[3:1] else rep(palette(), 
                                                                               length = n.groups), pch = 1:n.groups, lwd = 1, lty = 1, 
          cex = par("cex"), cex.axis = par("cex.axis"), cex.labels = NULL, 
          cex.main = par("cex.main"), legend.plot = length(levels(groups)) > 
            1, legend.pos = NULL, row1attop = TRUE, ...) 
{
  if (id.method[1] == "identify") 
    stop("interactive point identification not permitted")
  legend.pos
  family <- match.arg(family)
  use <- match.arg(use)
  na.action <- if (use == "complete.obs") 
    na.omit
  else na.pass
  if (missing(labels)) {
    labels <- rownames(x)
    if (is.null(labels)) 
      labels <- as.character(seq(length.out = nrow(x)))
  }
  if (!(missing(groups))) {
    x <- na.action(data.frame(groups, labels, x, stringsAsFactors = FALSE))
    groups <- x$groups
    if (!is.factor(groups)) 
      groups <- as.factor(as.character(x[, 1]))
    labels <- x[, 2]
    x <- x[, -(1:2)]
  }
  else {
    x <- na.action(data.frame(labels, x, stringsAsFactors = FALSE))
    labels <- x[, 1]
    x <- x[, -1]
  }
  if (missing(nclass)) 
    nclass <- "FD"
  legendPlot <- function(position = "topright") {
    usr <- par("usr")
    legend(position, bg = "white", legend = levels(groups), 
           pch = pch, col = col[1:n.groups], cex = cex)
  }
  do.legend <- legend.plot
  panel.density <- function(x, ...) {
    if (n.groups > 1) {
      levs <- levels(groups)
      for (i in 1:n.groups) {
        xx <- x[levs[i] == groups]
        dens.x <- try(density(xx, adjust = adjust, na.rm = TRUE), 
                      silent = TRUE)
        if (!inherits(dens.x, "try-error")) {
          lines(dens.x$x, min(x, na.rm = TRUE) + dens.x$y * 
                  diff(range(x, na.rm = TRUE))/diff(range(dens.x$y, 
                                                          na.rm = TRUE)), col = col[i])
        }
        else warning("cannot estimate density for group ", 
                     levs[i], "\n", dens.x, "\n")
        rug(xx, col = col[i])
      }
    }
    else {
      dens.x <- density(x, adjust = adjust, na.rm = TRUE)
      lines(dens.x$x, min(x, na.rm = TRUE) + dens.x$y * 
              diff(range(x, na.rm = TRUE))/diff(range(dens.x$y, 
                                                      na.rm = TRUE)))
      rug(x)
    }
    if (do.legend) 
      legendPlot(position = if (is.null(legend.pos)) 
        "topright"
        else legend.pos)
    do.legend <<- FALSE
  }
  panel.histogram <- function(x, ...) {
    par(new = TRUE)
    h.col <- col[1]
    if (h.col == "black") 
      h.col <- "gray"
    hist(x, main = "", axes = FALSE, breaks = nclass, col = h.col)
    if (do.legend) 
      legendPlot(position = if (is.null(legend.pos)) 
        "topright"
        else legend.pos)
    do.legend <<- FALSE
  }
  panel.boxplot <- function(x, ...) {
    b.col <- col[1:n.groups]
    b.col[b.col == "black"] <- "gray"
    par(new = TRUE)
    if (n.groups == 1) 
      boxplot(x, axes = FALSE, main = "", col = col[1])
    else boxplot(x ~ groups, axes = FALSE, main = "", col = b.col)
    if (do.legend) 
      legendPlot(position = if (is.null(legend.pos)) 
        "topright"
        else legend.pos)
    do.legend <<- FALSE
  }
  panel.oned <- function(x, ...) {
    range <- range(x, na.rm = TRUE)
    delta <- diff(range)/50
    y <- mean(range)
    if (n.groups == 1) 
      segments(x - delta, x, x + delta, x, col = col[3])
    else {
      segments(x - delta, x, x + delta, x, col = col[as.numeric(groups)])
    }
    if (do.legend) 
      legendPlot(position = if (is.null(legend.pos)) 
        "bottomright"
        else legend.pos)
    do.legend <<- FALSE
  }
  panel.qqplot <- function(x, ...) {
    par(new = TRUE)
    if (n.groups == 1) 
      qqnorm(x, axes = FALSE, xlab = "", ylab = "", main = "", 
             col = col[3])
    else qqnorm(x, axes = FALSE, xlab = "", ylab = "", main = "", 
                col = col[as.numeric(groups)])
    qqline(x, col = col[1])
    if (do.legend) 
      legendPlot(position = if (is.null(legend.pos)) 
        "bottomright"
        else legend.pos)
    do.legend <<- FALSE
  }
  panel.blank <- function(x, ...) {
    if (do.legend) 
      legendPlot(if (is.null(legend.pos)) 
        "topright"
        else legend.pos)
    do.legend <<- FALSE
  }
  if (!missing(smooth)) {
    smoother <- if (isTRUE(smooth)) 
      loessLine
    else FALSE
  }
  if (!missing(span)) 
    smoother.args$span <- span
  which.fn <- match(match.arg(diagonal), c("density", "boxplot", 
                                           "histogram", "oned", "qqplot", "none"))
  diag <- list(panel.density, panel.boxplot, panel.histogram, 
               panel.oned, panel.qqplot, panel.blank)[[which.fn]]
  groups <- as.factor(if (missing(groups)) 
    rep(1, length(x[, 1]))
    else groups)
  counts <- table(groups)
  if (any(counts == 0)) {
    levels <- levels(groups)
    warning("the following groups are empty: ", paste(levels[counts == 
                                                               0], collapse = ", "))
    groups <- factor(groups, levels = levels[counts > 0])
  }
  n.groups <- length(levels(groups))
  if (n.groups > length(col)) 
    stop("number of groups exceeds number of available colors")
  if (length(col) == 1) 
    col <- rep(col, 3)
  if (transform != FALSE | length(transform) == ncol(x)) {
    if (transform == TRUE & length(transform) == 1) {
      transform <- if (by.groups) 
        coef(powerTransform(as.matrix(x) ~ groups, family = family), 
             round = TRUE)
      else coef(powerTransform(x, family = family), round = TRUE)
    }
    for (i in 1:ncol(x)) {
      x[, i] <- if (family == "bcPower") 
        bcPower(x[, i], transform[i])
      else yjPower(x[, i], transform[i])
      var.labels[i] <- paste(var.labels[i], "^(", round(transform[i], 
                                                        2), ")", sep = "")
    }
  }
  labs <- labels
  pairs(x, labels = var.labels, cex.axis = cex.axis, cex.main = cex.main, 
        cex.labels = cex.labels, cex = cex, diag.panel = diag, 
        row1attop = row1attop, panel = function(x, y, ...) {
          for (i in 1:n.groups) {
            subs <- groups == levels(groups)[i]
            if (plot.points) 
              points(x[subs], y[subs], pch = pch[i], col = col[if (n.groups == 
                                                                   1) 
                3
                else i], cex = cex)
            if (by.groups) {
              if (is.function(smoother)) 
                smoother(x[subs], y[subs], col = col[i], 
                         log.x = FALSE, log.y = FALSE, spread = spread, 
                         smoother.args = smoother.args)
              if (is.function(reg.line)) 
                reg(reg.line, x[subs], y[subs], lty = lty, 
                    lwd = lwd, log.x = FALSE, log.y = FALSE, 
                    col = col[i])
              if (ellipse) 
                dataEllipse(x[subs], y[subs], plot.points = FALSE, 
                            levels = levels, col = col[i], robust = robust, 
                            lwd = 1, center.cex = 0.5)
              showLabels(x[subs], y[subs], labs[subs], id.method = id.method, 
                         id.n = id.n, id.col = col[i], id.cex = id.cex, 
                         id.location = id.location, all = list(labels = labs, 
                                                               subs = subs))
            }
          }
          if (!by.groups) {
            if (is.function(reg.line)) 
              abline(reg.line(y ~ x), lty = lty, lwd = lwd, 
                     col = col[1])
            if (is.function(smoother)) 
              smoother(x, y, col = col[2], log.x = FALSE, 
                       log.y = FALSE, spread = spread, smoother.args = smoother.args)
            if (ellipse) 
              dataEllipse(x, y, plot.points = FALSE, levels = levels, 
                          col = col[1], robust = robust, lwd = 1, center.cex = 0.5)
            showLabels(x, y, labs, id.method = id.method, 
                       id.n = id.n, id.col = id.col, id.location = id.location, 
                       id.cex = id.cex)
          }
        }, ...)
}