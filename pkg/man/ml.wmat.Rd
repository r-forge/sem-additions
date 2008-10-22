\name{ml.wmat}
\alias{ml.wmat}
\title{Weight Matrix from Fml}
\description{ Normal Theory Weight Matrix from Maximum Liklihood Criterion }

\usage{ml.wmat(sem.obj, useFit=F)}
\arguments{
  \item{sem.obj}{A fitted \code{\link{sem}} object}
  \item{useFit}{Whether to use the fitted covariance matrix or the observed covariance matrix.  Defaults to the observed matrix.}
}

\details{ Calculates the normal theory weight matrix (W) for the maximum liklihood fit using the following equation:

\deqn{W = \frac{1}{2} D_p' (A\otimes A) D_p}{W = 1/2 * Dp' * kronecker(A, A) * Dp} 

where \eqn{Dp} is the duplication matrix of  order p (p = # of observed variables) and \eqn{A} is the inverse of a matrix that coverges to the population covariance matrix.  In practice this is often either the sample or fitted covariance matrix.  

}


\seealso{
	\code{\link{sem}}.
}

\references{
Browne, M.W.  1974.  Generalized least Square estimators in the analysis of covariance structures. \emph{South African Statistical Journal}. 8: 1-24

Satorra, A. and Bentler, P. M.  1988.  Scaling corrections for chi-square statistics in covariance structure analysis. American Statistical Association.  \emph{Proceedings of the Business and Economic Statistics Section}.  308-313.

}