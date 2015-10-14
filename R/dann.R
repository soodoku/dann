#' dann
#'
#' Main function 
#' 
#' @param x  matrix
#' @param testx test
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
#' @usage \dontrun{
#' dann(x <- matrix(rnorm(120,1,.2)), testx <- glass.test$x, y <- matrix(rnorm(120,1,.5)), 
#' epsilon = 1, fullw = FALSE, iter = 100,  covmin = 1e-04, cv = FALSE)
#' }

dann <- 
function(x, testx = matrix(double(p), nrow = 1), y, k = 5, 
				kmetric = max(50, 0.2 * n), epsilon = 1, fullw = FALSE, scalar = FALSE, iter = 1, 
				covmin = 1e-04, cv = FALSE) 
{
	# tester: epsilon = 1; rate = 0.5; fullw = FALSE; scalar = FALSE; iter = 1; covmin = 1e-04; cv = FALSE
	# k=5; x <- matrix(rnorm(120,1,.2)); y <- matrix(rnorm(120,1,.5)); testx <- glass.test$x; ntest <- nrow(testx)
	# kmetric = max(50, 0.2 * n)
	storage.mode(x) 	<- "double"
	storage.mode(testx) <- "double"
	storage.mode(y) 	<- "integer"
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
	.Fortran("dann", as.integer(np[1]), as.integer(np[2]), x, 
			y, as.integer(nclass), t(testx), as.logical(cv), as.integer(ntest), 
			predict = matrix(integer(ntest * neps), ntest, neps, 
					dimnames = list(NULL, format(round(epsilon, 5)))), 
			as.integer(kmetric), as.integer(k), as.integer(iter), 
			fullw, scalar, epsilon = epsilon, as.integer(neps), integer(n), 
			double(n), matrix(double(p^2), p, p), as.double(covmin), 
			matrix(double(nclass * p), nclass, p), double(n), as.single(runif(ntest)), 
			double(n + 2 * p^2 + 3 * p))$predict
}
