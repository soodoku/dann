#' knncv
#'
#' Cross-validated k-means
#' 
#' @param x matrix  
#' @param y labels
#' @param k number of clusters
#'
#' @return list of standardized matrices along with scaling information (centers)
#' @export
#' @examples \dontrun{
#' knncv(x, y, k=5)
#' }

knncv <- function(x, y, k = 5) {
  storage.mode(x) <- "double"
  storage.mode(y) <- "integer"
  x <- as.matrix(x)
  np <- dim(x)
  p <- np[2]
  n <- np[1]
  
  if (!is.loaded("knncv")) {
    dyn.load(paste(.libPaths()[1], "/dann/libs/dann.so", sep = ""))
  }
  
  junk <- .Fortran("knncv", as.integer(np[1]), as.integer(np[2]), x, y, predict = integer(n), error = integer(1), as.integer(k), as.single(runif(n)), double(n), PACKAGE = "dann")
  
  res <- junk$predict
  attr(res, "error") <- junk$error
  
  res
}
