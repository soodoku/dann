mydann2 <-
function (train.data, test.data, 
    p = dim(x)[2], kmetric = max(50, 0.2 * 160), ktarget = 5, 
	epsilon.list = c(1, 0.1, 0.3, 0.5, 0.8, 0.9, 1, 2), iter.list = 1, ...) 
{
    x <- train.data$x
    x <- x[, seq(p)]
    y <- train.data$y
    mm <- apply(x, 2, var)
    x <- scale(x, F, sqrt(mm))
    xx <- test.data$x
    xx <- xx[, seq(p)]
    xx <- scale(xx, F, sqrt(mm))
    results <- matrix(0, length(iter.list), length(epsilon.list))
    dimnames(results) <- list(format(round(iter.list, 0)), format(round(epsilon.list, 2)))
    for (i in seq(along = iter.list)) {
        iter <- iter.list[i]
        a <- dann2(x, xx, y, kmetric = kmetric, k = ktarget, 
            epsilon = epsilon.list, iter = iter, ...)
        aa <- apply(a != test.data$y, 2, sum)
        cat("iter", iter, "error", aa, "\n")
        results[i, ] <- aa
    }
    results
    kk <- knn(x, xx, y, k = 5)
    knnerror <- sum(kk != test.data$y)
    attr(results, "5nn") <- knnerror
    results
}
