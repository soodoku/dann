#' nnsubspace
#'
#' @param x
#' @param y
#' @param k  
#' @param epsilon 
#' @param fullw
#' @param iter
#' @param cv
#'
#' @return A list with items including Name of the Application, No. of pages remaining (given the money), 
#' No. of fields remaining (given the money), and when the application credits expire. 
#' @export
#' @usage # dannsub()

nnsubspace <-
function (x, y, 
    k = length(y)/2, epsilon = 1, fullw = FALSE, iter = 1, cv = 0) 
{
    storage.mode(x) <- "double"
    storage.mode(x0) <- "double"
    storage.mode(y) <- "integer"
    nclass <- length(table(y))
    np <- dim(x)
    p <- np[2]
    n <- np[1]
    between <- matrix(0, p, p)
    for (i in seq(n)) {
        junk <- .Fortran("nndist", as.integer(np[1]), as.integer(np[2]), 
            as.integer(nclass), as.integer(k), x, x[i, ], 
            as.integer(cv), y, as.integer(iter), fullw, T, epsilon = as.double(epsilon), 
            which = integer(n), dist = double(n), covw = matrix(double(p^2), 
                p, p), covmin = as.double(1e-04), means = matrix(double(nclass * 
                p), nclass, p), weight = double(n), values = double(p), 
            vectors = double(p * p), double(p * p), double(n + 
                2 * p), PACKAGE ="dann")$means
        between <- between + t(junk) %*% junk
        cat(i, " ")
    }
    between/n
}
