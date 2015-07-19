\name{knn}
\alias{knn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
knn
}
\description{
Implements K Nearest Neighbors
}
\usage{
knn(train, test, cl, k = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{train}{
%%     ~~Describe \code{train} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{cl}{
%%     ~~Describe \code{cl} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

glass <-
function (fullw = FALSE, iter = 1, kmet = 50) 
{
	data(dannt)
	#attach(paste(.libPaths()[1], "dann/data/", "dann.RData", sep=""))
	set.seed(301)
    nte <- 96
    xs <- glass.train$x
    xxs <- glass.test$x
    y <- glass.train$y
    yy <- glass.test$y
    yhat2 <- rep(0, nte)
    yhat3 <- yhat2
    yhat1 <- knn(xs, xxs, y, 5)
    a <- dann(xs, xxs, y, kmet = kmet, epsilon = epsilon.list, fullw = fullw, iter = iter)
    yhat2 <- a
    ju <- fda(y ~ xs)
    yhat <- predict(ju, xxs)
    #cat(c("glass", eps, diagW), fill = TRUE)
    cat(c("lda", sum(yhat != yy)), fill = TRUE)
    cat(c("5nn", sum(yhat1 != yy)), fill = TRUE)
    apply(yhat2 != yy, 2, sum)
}
glass()
}
