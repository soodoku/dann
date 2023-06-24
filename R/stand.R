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

stand <- function(x = NA, xx = NA) {
  if (is.na(x) && is.na(xx)) {
    stop("Both input matrices 'x' and 'xx' are missing.")
  }
  
  if (!is.na(x)) {
    mm <- apply(x, 2, mean)
    dd <- sqrt(apply(x, 2, var))
    x <- scale(x, center = mm, scale = dd)
  }
  
  if (!is.na(xx)) {
    if (is.na(x)) {
      stop("Input matrix 'x' is missing. Cannot perform standardization on 'xx' without 'x'.")
    }
    
    mm <- apply(xx, 2, mean)
    dd <- sqrt(apply(xx, 2, var))
    xx <- scale(xx, center = mm, scale = dd)
  }
  
  return(list(x = x, xx = xx))
}
