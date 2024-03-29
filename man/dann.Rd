% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dann.R
\name{dann}
\alias{dann}
\title{dann}
\usage{
dann(x, testx = matrix(double(p), nrow = 1), y, k = 5, kmetric = max(50,
  0.2 * n), epsilon = 1, fullw = FALSE, scalar = FALSE, iter = 1,
  covmin = 1e-04, cv = FALSE)
}
\arguments{
\item{x}{covariates matrix}

\item{testx}{test covariate matrix}

\item{y}{labels}

\item{k}{number of clusters}

\item{kmetric}{metric}

\item{epsilon}{epsilon}

\item{fullw}{Boolean}

\item{scalar}{Boolean}

\item{iter}{maximum number of iterations}

\item{covmin}{cov}

\item{cv}{boolean reflecting whether to cross-validate or not}
}
\value{
A list with items including Name of the Application, No. of pages remaining (given the money), 
No. of fields remaining (given the money), and when the application credits expire.
}
\description{
Run Discriminant Adaptive Nearest Neighbors
}
\examples{
\dontrun{
dann(x <- matrix(rnorm(120,1,.2)), testx <- glass.test$x, y <- matrix(rnorm(120,1,.5)), 
epsilon = 1, fullw = FALSE, iter = 100,  covmin = 1e-04, cv = FALSE)
}
}

