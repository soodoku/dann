#' dann
#'
#' Run Discriminant Adaptive Nearest Neighbors 
#' 
#' @param x     covariates matrix
#' @param testx test covariate matrix
#' @param y  labels
#' @param k  number of clusters
#' @param kmetric metric
#' @param epsilon epsilon
#' @param fullw  Boolean
#' @param scalar Boolean
#' @param iter   maximum number of iterations
#' @param covmin cov
#' @param cv boolean reflecting whether to cross-validate or not
#'
#'
#' @return A list with items including Name of the Application, No. of pages remaining (given the money), 
#' No. of fields remaining (given the money), and when the application credits expire. 
#' 
#' @export
#' 
#' @examples \dontrun{
#' dann(x <- matrix(rnorm(120,1,.2)), testx <- glass.test$x, y <- matrix(rnorm(120,1,.5)), 
#' epsilon = 1, fullw = FALSE, iter = 100,  covmin = 1e-04, cv = FALSE)
#' }

dann <- function(x, testx = matrix(nrow = 1, ncol = p), y, k = 5,
                 kmetric = max(50, 0.2 * n), epsilon = 1, fullw = FALSE, scalar = FALSE, iter = 1,
                 covmin = 1e-04, cv = FALSE) {
  
  storage.mode(x)  <- "double"
  storage.mode(testx) <- "double"
  storage.mode(y)  <- "integer"
  
  np <- dim(x)
  p <- np[2]
  n <- np[1]
  
  storage.mode(epsilon) <- "double"
  neps <- length(epsilon)
  nclass <- length(table(y))
  
  if (cv) {
    ntest <- n
  } else {
    ntest <- nrow(testx)
  }
  
  pred <- matrix(integer(ntest * neps), nrow = ntest, ncol = neps,
                 dimnames = list(NULL, format(round(epsilon, 5))))
  
  .Fortran("dann",
           np[1], np[2], x, y, nclass, t(testx), cv, ntest,
           pred, kmetric, k, iter, fullw, scalar, epsilon,
           neps, integer(n), double(n), matrix(double(p^2), nrow = p, ncol = p),
           covmin, matrix(double(nclass * p), nrow = nclass, ncol = p),
           double(n), as.single(runif(ntest)),
           double(n + 2 * p^2 + 3 * p), PACKAGE = "dann")$pred
}
