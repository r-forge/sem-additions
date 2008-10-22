\name{delta_matrix}
\alias{delta_matrix}
\title{Delta Matrix}
\description{ Calculated the delta matrix for sem objects }

\usage{delta_matrix(sem.object, adj=1e-04)}
\arguments{
  \item{sem.object}{A fitted object from \code{\link{sem}}.}
  \item{adj}{The adjustment to each parameter value used to calculate the change in the covariance matrix.}
}

\details{Calculates the Jacobian matrix for an sem object of d(sigma)/d(theta) where sigma is vech(Sigma), the fitted covariance matrix, and theta is the estimate of parameter values.  Each column of the returned matrix corresponds to a path or variance/covariance estimate.  Each row is one unique cell from Sigma.

}


\seealso{
	\code{\link{sem}}.
}
