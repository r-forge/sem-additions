\name{sbMeanChisq}
\alias{sbMeanChisq}
\title{Satorra-Bentler Mean Chi Square}
\description{ I needed to write more code to make working with sem easier. }

\usage{sbMeanChisq(sem.obj, sem.data, ...}
\arguments{
  \item{sem.obj}{A fitted \code{\link{sem}}. object}
  \item{sem.data}{A data frame of variables used to generate the covariance matrix used in fitting the \code{\link{sem}}. object.}
  \item{...}{Additional arguments to \code{\link{sbchisq}}}
}


\details{This function calculates an adjusted statistic, Tbarbar, based off of the Satorra-Bentler Chi Squared statistic from\code{\link{sbchisq}}.  Here, a corrected decrees of freedom (dc) is calculated such that
\deqn{dc= trace(U*G)^2 / trace( (U*G)^2 )}

where U and G are both defined in the same manner as in \code{\link{sbchisq}}.  the correction factor, c, is then recalculated with the corrected degrees of freedom and \eqn{Tbarbar = T/c}, just as with the ordinary SB Chi Squared value.
}

\value{ An object of class \code{\link{adjchisq}} with a corrected degrees of freedom.}

\seealso{
	\code{\link{sbchisq}}
}


\references{
Bentler, P.M. and Dudgeon, P.  1996.  Covariance structure analysis: Statistical practice, theory, and directions.  \emph{Annual Review of Psychology}.  47:563-592.

Satorra, A. and Bentler, P. M.  1988.  Scaling corrections for chi-square statistics in covariance structure analysis. American Statistical Association.  \emph{Proceedings of the Business and Economic Statistics Section}.  308-313.

}