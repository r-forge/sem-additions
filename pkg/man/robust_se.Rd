\name{robust_se}
\alias{robust_se}
\title{Robust Standard Errors}
\description{ Calculates Robust Stanard Errors for Structural Equation Models }

\usage{robust_se(sem.obj, adj.obj=NA, data.obj=NA, useFit=F)}
\arguments{
  \item{sem.obj}{A fitted \code{\link{sem}} object.}
  \item{adj.obj}{An \code{\link{adjchisq}} adjusted chi squared object fit using \code{\link{sbchisq}}, or, NA if you wish to have the Satorra-Bentler Chi Squared value calculated}
  \item{data.obj}{If no adjusted chi squared object is provided, a data from from which to calculate the Satorra-Bentler Chi Squared index and its associated matrices.}
    \item{useFit}{Use the observed or fitted covariance matrix for calculation of the maximum liklihood weight matrix.  Defaults to False.}

}

\details{This function applies Satorra and Bentler's correction for the effects of non-normal data on the estimates of path standard errors.  A new asymptotic covariance matrix for the parameters is calculated such that the standard errors = sqrt(diag(acov))/ (n-1).  First, \code{robust_se} uses \code{\link{sbchisq}} to calculate the ADF weight matrix G, the ML weight matrix W, and the jacobian matrix D (see links therein for further description).  We then calculate the information matrix, H, as \eqn{H = inv(D' * W * D)}.  The asymptotic covariance matrix is then calculated as

\deqn{ acov = H * ( D' * W * G * W * D) * H}


}

\value{ An vector of standard errors.}

\seealso{
	\code{\link{sem}}
	\code{\link{sbchisq}}
	\code{\link{adjchisq}}
	\code{\link{robust_summary}}
}


\references{
Bentler, P.M. and Dudgeon, P.  1996.  Covariance structure analysis: Statistical practice, theory, and directions.  \emph{Annual Review of Psychology}.  47:563-592.


}