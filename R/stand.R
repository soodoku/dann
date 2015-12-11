#' stand
#'
#' Standardize each column of both of the matrices with mean and standard deviation derived from the first matrix.
#' 
#' @param x  matrix
#' @param xx another matrix, with same number of columns as x
#'
#' @return list of standardized matrices x and xx along with scaling information --- mean and standard deviation of original columns
#'
#' @export
#' @examples stand(matrix(1:6, ncol=3), matrix(1:6, ncol=3))

stand <-
function (x = NA, xx = NA) 
{
    mm <- apply(x, 2, mean)
    dd <- sqrt(apply(x, 2, var))
    x  <- scale(x, mm, dd)
    xx <- scale(xx, mm, dd)
    return(list(x = x, xx = xx))
}
