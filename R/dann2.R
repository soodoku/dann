dann2 <-
function (x, testx = matrix(double(p), nrow = 1), y, k = 5, kmetric = length(y)/2, 
    epsilon = 1, rate = 0.5, fullw = FALSE, scalar = FALSE, iter = 1, 
    covmin = 1e-04, cv = FALSE) 
{
	# tester: epsilon = 1; rate = 0.5; fullw = FALSE; scalar = FALSE; iter = 1; covmin = 1e-04; cv = FALSE
    # k=5; x <- matrix(rnorm(120,1,.2)), y <- matrix(rnorm(120,1,.5)), testx <- 
    storage.mode(x) <- "double"
    storage.mode(testx) <- "double"
    storage.mode(y) <- "integer"
    nclass <- length(table(y))
    storage.mode(epsilon) <- "double"
    neps <- length(epsilon)
    np <- dim(x)
    p <- np[2]
    n <- np[1]
    storage.mode(testx) <- "double"
    if (cv) 
        ntest <- n
    else ntest <- nrow(testx)
    if (!is.loaded("dann2")) 
		dyn.load(paste(.libPaths()[1], "/dann/libs/dann.so", sep=""))
    .Fortran("dann2", as.integer(np[1]), as.integer(np[2]), x, 
        y, as.integer(nclass), t(testx), as.logical(cv), as.integer(ntest), 
        predict = matrix(integer(ntest * neps), ntest, neps, 
            dimnames = list(NULL, format(round(epsilon, 5)))), 
        as.integer(c(kmetric, k, iter)), c(fullw, scalar), epsilon = epsilon, 
        as.integer(neps), integer(n), double(n), matrix(double(p^2), 
            p, p), matrix(double(p^2), p, p), as.double(c(rate, 
            epsilon[1], covmin)), matrix(double(nclass * p), 
            nclass, p), double(n), as.single(runif(ntest)), double(n + 
            2 * p^2 + 3 * p), x, y, PACKAGE ="dann")$predict
}