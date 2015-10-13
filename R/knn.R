#' knn
#'
#' K-nearest Neighbors
#' 
#' @param train  matrix
#' @param test test
#' @param cl  number of clusters
#' @param k  number of clusters
#'
#'
#' @return A list with items including Name of the Application, No. of pages remaining (given the money), 
#' No. of fields remaining (given the money), and when the application credits expire. 
#' 
#' @export
#' 
#' @usage \dontrun{ 
#' knn()
#'}

knn <- function (train, test, cl, k = 1) 
{
    train <- as.matrix(train)
    test <- as.matrix(test)
    p <- dim(train)[2]
    ntr <- dim(train)[1]
    if (length(cl) != ntr) 
        stop("train and class have different lengths")
    nte <- dim(test)[1]
    if (dim(test)[2] != p) 
        stop("Dims of test and train differ")
    if (!is.loaded("knn")) 
		dyn.load(paste(.libPaths()[1], "/dann/libs/dann.so", sep=""))
    if (is.factor(cl)) 
        cl1 <- as.numeric(cl)
    else cl1 <- cl
    z <- .Fortran("knn", as.integer(k), as.integer(ntr), as.integer(nte), 
        as.integer(p), as.double(train), as.integer(cl1), as.double(test), 
        res = integer(nte), u = as.single(runif(nte)), d = double(nte), PACKAGE ="dann")
    res <- z$res
    if (is.factor(cl)) {
        levels(res) <- levels(cl)
        res <- as.factor(res)
    }
    res
}