#' stand
#'
#' Standardize each column of the matrix
#' 
#' @param x  matrix
#' @param xx matrix with same number of columns as x
#'
#' @return list of standardized matrices along with scaling information (centers)
#' @export
#' @usage stand(matrix(1:6, ncol=3), matrix(1:6, ncol=3))

stand <-
function (x = NA, xx = NA) 
{
    mm <- apply(x, 2, mean)
    dd <- sqrt(apply(x, 2, var))
    x  <- scale(x, mm, dd)
    xx <- scale(xx, mm, dd)
    return(list(x = x, xx = xx))
}
