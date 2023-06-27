mydannsubauto <- function(train.data, test.data, p = dim(train.data$x)[2], kmetric = max(0.2 * length(train.data$y), 50), epsilon.list = c(1, 0.5, 2, 5), plus = 1, ...) {
  min2d <- function(mat) {
    i <- order(mat)[1]
    dd <- dim(mat)
    i1 <- trunc(i / dd[1]) + 1
    i2 <- i %% dd[1]
    if (i2 == 0)
      i2 <- dd[1]
    c(i2, i1)
  }
  
  x <- train.data$x[, seq(p)]
  y <- train.data$y
  n <- length(y)
  mm <- apply(x, 2, var)
  x <- scale(x, center = FALSE, scale = sqrt(mm))
  xx <- test.data$x[, seq(p)]
  xx <- scale(xx, center = FALSE, scale = sqrt(mm))
  trans <- diag(p)
  oldd <- p + 1
  newd <- p
  
  while (newd < oldd) {
    cat("new dimension", newd, "\n")
    junk <- dannsub(x, y, km = kmetric, k = 5, epsilon = epsilon.list, fullw = FALSE, scalar = TRUE, dims = seq(ncol(x)), iter.sub = 1, iter = 1, method = "dann")
    dd <- min2d(junk$error)
    oldd <- newd
    newd <- min(dd[1] + plus, ncol(x))
    uptrans <- junk$vectors[, seq(newd), drop = FALSE] / junk$scale
    x <- x %*% uptrans
    trans <- trans %*% uptrans
    print(junk$error)
  }
  
  nn <- min2d(junk$error)[1]
  cat("chosen dimension", nn, "\n")
  xx <- xx %*% trans[, seq(nn), drop = FALSE]
  x <- x[, seq(nn), drop = FALSE]
  junk <- list(errors = apply(test.data$y != dann2(x, testx = xx, y = y, kmetric = kmetric, epsilon = epsilon.list, cv = FALSE), 2, sum))
  kk <- knn(x, xx, y, k = 5)
  knnerror <- sum(kk != test.data$y)
  junk[["5nn(reduced space)"]] <- knnerror
  junk
}
