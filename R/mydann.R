#' mydann
#'
#' @param train.data training data. All the features in matrix x. and class labels in y. 
#' @param test.data  test data. Same format as the test data
#' @param p dimensions of x
#' @param kmetric k met
#' @param k Number of nearest neighbors
#' @param epsilon.list epsilon
#' @param iter.list iteration
#' 
#' @return  results
#' @examples \dontrun{ 
#' mydann()
#' }

mydann <- 
function (train.data, test.data, p = dim(x)[2], kmetric = max(50, 0.2 * n), k = 5, epsilon.list = c(1, 0.5, 2, 5), iter.list = 1, ...) 
{
    this.call <- match.call()
    x <- train.data$x
    x <- x[, seq(p)]
    y <- as.integer(factor(train.data$y))
    yy <- as.integer(factor(test.data$y))
    n <- length(y)
    mm <- apply(x, 2, var)
    x <- scale(x, F, sqrt(mm))
    xx <- test.data$x
    xx <- xx[, seq(p)]
    xx <- scale(xx, F, sqrt(mm))
    results <- matrix(0, length(iter.list), length(epsilon.list))
    dimnames(results) <- list(format(round(iter.list, 0)), format(round(epsilon.list, 2)))
    for (i in seq(along = iter.list)) {
        iter <- iter.list[i]
        a <- dann(x, xx, y, kmetric = kmetric, k = k, epsilon = epsilon.list, 
            iter = iter, ...)
        aa <- apply(a != yy, 2, sum)
        cat("iter", iter, "error", aa, "\n")
        results[i, ] <- aa
    }
    results
    kk <- knn(x, xx, y, k = 5)
    knnerror <- sum(kk != yy)
    attr(results, "5nn") <- knnerror
    attr(results, "call") <- this.call
    results
}