\name{robust_summary}
\alias{robust_summary}
\title{Robust Summary}
\description{ I needed to write more code to make working with sem easier. }

\usage{robust_summary(sem.obj, adj.obj=NA, data.obj=NA)}
\arguments{
 \item{sem.obj}{A fitted \code{\link{sem}} object.}
  \item{adj.obj}{An \code{\link{adjchisq}} adjusted chi squared object fit using \code{\link{sbchisq}}, or, NA if you wish to have the Satorra-Bentler Chi Squared value calculated}
  \item{data.obj}{If no adjusted chi squared object is provided, a data from from which to calculate the Satorra-Bentler Chi Squared index and its associated matrices.}
    \item{useFit}{Use the observed or fitted covariance matrix for calculation of the maximum liklihood weight matrix.  Defaults to False.}

  }

\details{Provides a summary using methods robust to violations of non-normality in the data.  Chi Squared and other statistics are corrected using the Satorra-Bentler correction.  Standard errors for z tests are provided by \code{\link{robust_se}}.
}


\seealso{
	\code{\link{sem}}
	\code{\link{sbchisq}}
	\code{\link{adjchisq}}
	\code{\link{robust_se}}
}


\references{
Bentler, P.M. and Dudgeon, P.  1996.  Covariance structure analysis: Statistical practice, theory, and directions.  \emph{Annual Review of Psychology}.  47:563-592.

Satorra, A. and Bentler, P. M.  1988.  Scaling corrections for chi-square statistics in covariance structure analysis. American Statistical Association.  \emph{Proceedings of the Business and Economic Statistics Section}.  308-313.

}