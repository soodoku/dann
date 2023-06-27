mydannsub <- function(train.data, test.data, p = dim(train.data$x)[2], kmetric = max(50, 0.2 * nrow(train.data$x)), k = 5, epsilon.list = c(1, 0.5, 2, 5), iter.list = 1, dims = seq(p), ...) {
  this.call <- match.call()
  x <- train.data$x
  x <- x[, seq(p)]
  y <- train.data$y
  n <- length(y)
  mm <- apply(x, 2, var)
  x <- scale(x, center = FALSE, scale = sqrt(mm))
  xx <- test.data$x
  xx <- xx[, seq(p)]
  xx <- scale(xx, center = FALSE, scale = sqrt(mm))
  junk <- dannsub(x, y, km = kmetric, k = k, epsilon = epsilon.list, fullw = FALSE, scalar = TRUE, dims = dims, iter.sub = 1, iter = 1, method = "dann", xnew = xx, ynew = test.data$y)
  kk <- knn(x, xx, y, k = k)
  knnerror <- sum(kk != test.data$y)
  junk$call <- this.call
  junk$"5nn" <- knnerror
  junk
}

