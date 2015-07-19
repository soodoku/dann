\name{dann-package}
\alias{dann-package}
\alias{dann}
\docType{package}
\title{
dann package
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab dann\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-03-15\cr
License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Who wrote it

Maintainer: Who to complain to <yourfault@somewhere.net>
~~ The author and/or maintainer of the package ~~
}
\references{
See Hastie and Tibshirani
}
\keyword{ dann, fda, lda, classification }
\seealso{
}
\examples{
##	Vowel
################################
vowel <-
		function (fullw = FALSE, iter = 1) 
{
	data(dannt)
	#attach(paste(.libPaths()[1], "dann/data/", "dann.RData", sep=""))
	Vowel[,1] <- as.numeric(Vowel[,1]) -1
	vowel.test <- Vowel
	vowel.train <- Vowel[463:990,]
	set.seed(301)
	nte <- 462
	x <- vowel.train[, 1:10]
	y <- vowel.train[, 11]
	xx <- vowel.test[1:nte, 1:10]
	yy <- vowel.test[1:nte, 11]
	junk <- stand(x, xx)
	xs <- junk$x
	xxs <- junk$xx
	yhat2 <- rep(0, nte)
	yhat3 <- yhat2
	yhat4 <- yhat3
	yhat5 <- yhat2
	yhat1 <- knn(xs, xxs, y, 5)
	a <- dann(xs, xxs, y, kmet = 100, epsilon = epsilon.list, fullw = fullw, iter = iter)
	yhat3 <- a
	ju <- fda(y ~ xs)
	yhat <- predict(ju, xxs)
	#cat(c("vowel", eps, diagW), fill = TRUE)
	cat(c("lda", sum(yhat != yy)), fill = TRUE)
	#cat(c("5nn", sum(yhat1 != yy)), fill = TRUE)
	#detach(2)
	apply(yhat3 != yy, 2, sum)
}
vowel()
## Sph 
######################
sph <-
		function (fullw = FALSE, iter = 1) 
{
	#list(c("#attach(\"dann.RData\")", "#attach(\"dann.RData\")"))
	#attach(paste(.libPaths()[1], "dann/data/", "dann.RData", sep=""))
	data(dannt)
	set.seed(301)
	nte <- 500
	p <- 10
	x 	<- sphdata.4$sph.tr[, 1:p]
	y 	<- sphdata.4$sph.tr[, p + 1]
	xx 	<- sphdata.4$sph.te[1:nte, 1:p]
	yy 	<- sphdata.4$sph.te[1:nte, p + 1]
	yhat2 <- rep(0, nte)
	yhat3 <- yhat2
	junk <- stand(x, xx)
	xs <- junk$x
	xxs <- junk$xx
	yhat1 <- knn(xs, xxs, y, 5)
	a <- dann(xs, xxs, y, kmet = 50, epsilon = epsilon.list, 
			fullw = fullw, iter = iter)
	yhat3 <- a
	d <- knn(xs[, 1:4], xxs[, 1:4], y, 5)
	ju <- fda(y ~ xs)
	yhat <- predict(ju, xxs)
	#cat(c("sphere", eps, diagW), fill = TRUE)
	cat(c("lda", sum(yhat != yy)), fill = TRUE)
	cat(c("5nn", sum(yhat1 != yy)), fill = TRUE)
	cat(c("5nn/4", sum(d != yy)), fill = TRUE)
	apply(yhat3 != yy, 2, sum)
}
sph()

}
