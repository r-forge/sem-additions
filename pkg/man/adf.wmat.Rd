\name{adf.wmat}
\alias{adf.wmat}
\title{ADF Weight Matrix }
\description{ Calculated the fourth order moment matrix }

\usage{adf.wmat(raw_data)}
\arguments{
  \item{raw_data}{A data frame with the variables in the model of interest.}
}

\details{Calculates the fourth order moment matrix (G) for a data set x with j variables and n observations and a covariance matrix S. Satorra and Bentler (1988) define G for i=1,2,...n as

\deqn{G = 1/n * sum( (b(i) - bbar) ( b(i) - bbar))}

where

\deqn{b(i) = vech( x(i) * x(i)' )}

and

\deqn{bbar = E( b(i) * vech(S))}

Or, put another way, if for the data set \eqn{x} with j variables, z is a matrix of j*(j+1)/2 columns with each column equal to the product of one of the unique combinations of two variables of x.  Then

\deqn{ G = cov(z) }

This later formulation is computationally faster, and is how the ADF weight matrix is computed here.

}


\seealso{
	\code{\link{sem}}.
}

\references{
Bentler, P.M. and Dudgeon, P.  1996.  Covariance structure analysis: Statistical practice, theory, and directions.  \emph{Annual Review of Psychology}.  47:563-592.

Satorra, A. and Bentler, P. M.  1988.  Scaling corrections for chi-square statistics in covariance structure analysis. American Statistical Association.  \emph{Proceedings of the Business and Economic Statistics Section}.  308-313.

}