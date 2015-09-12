nnsubmeans <-
function (x, y, 
    k = 20, epsilon = 1, fullw = FALSE, iter = 1, cv = 0) 
{
    storage.mode(x) <- "double"
    storage.mode(x0) <- "double"
    storage.mode(y) <- "integer"
    nclass <- length(table(y))
    np <- dim(x)
    p <- np[2]
    n <- np[1]
    if (!is.loaded("nndist")) 
		dyn.load(paste(.libPaths()[1], "/dann/libs/dann.so", sep=""))
    bet <- matrix(0, n, 4)
    for (i in seq(n)) {
        junk <- .Fortran("nndist", as.integer(np[1]), as.integer(np[2]), 
            as.integer(nclass), as.integer(k), x, x[i, ], 
            as.integer(cv), y, as.integer(iter), fullw, T, epsilon = as.double(epsilon), 
            which = integer(n), dist = double(n), covw = matrix(double(p^2), 
                p, p), covmin = as.double(1e-04), means = matrix(double(nclass * 
                p), nclass, p), weight = double(n), values = double(p), 
            vectors = double(p * p), double(p * p), double(n + 
                2 * p), PACKAGE ="dann")$means
        bb <- eigen(t(junk) %*% junk)
        bb <- bb$vec[, 1] * sqrt(bb$val[1])
        bet[i, ] <- c(x[i, ] + 
            bb/2, x[i, ] - bb/2)
        cat(i, " ")
    }
    bet
}