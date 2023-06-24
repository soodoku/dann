#' nndist
#'
#' Main function 
#' 
#' @param x  matrix
#' @param y  y
#' @param x0  colMeans of x
#' @param k  number of clusters
#' @param epsilon epsilon
#' @param fullw  fullw
#' @param scalar boolean
#' @param iter   number of iterations
#' @param cv cross-validate
#'
#'
#' @return A list with items including Name of the Application, No. of pages remaining (given the money), 
#' No. of fields remaining (given the money), and when the application credits expire. 
#' 
#' @export
#' 
#' @examples \dontrun{
#' nndist()
#'}

nndist <- function(x, y, x0 = apply(x, 2, mean), k = length(y)/2, epsilon = 1, fullw = FALSE, scalar = FALSE, iter = 1, cv = 0) {
  if (is.matrix(x)) {
    storage.mode(x) <- "double"
  } else {
    stop("Input 'x' should be a matrix.")
  }
  
  if (is.matrix(x0)) {
    storage.mode(x0) <- "double"
  } else {
    stop("Input 'x0' should be a matrix.")
  }
  
  if (is.integer(y)) {
    storage.mode(y) <- "integer"
  } else {
    stop("Input 'y' should be an integer vector.")
  }
  
  nclass <- length(table(y))
  np <- dim(x)
  p <- np[2]
  n <- np[1]
  
  junk <- .Fortran("nndist", as.integer(np[1]), as.integer(np[2]), as.integer(nclass), as.integer(k), x, x0, as.integer(cv), y, as.integer(iter), fullw, scalar, epsilon = as.double(epsilon), which = integer(n), dist = double(n), covw = matrix(double(p^2), p, p), covmin = as.double(1e-04), means = matrix(double(nclass * p), nclass, p), weight = double(n), values = double(p), vectors = double(p * p), double(p * p), double(n + 2 * p), PACKAGE = "dann")
  
  return(junk)
}
