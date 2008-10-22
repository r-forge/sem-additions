\name{browneChisq}
\alias{browneChisq}
\title{Browne's Corrected Chi Squared Test}
\description{ I needed to write more code to make working with sem easier. }

\usage{browneChisq(sem.obj, sem.data, ...)}
\arguments{
 \item{sem.obj}{A fitted \code{\link{sem}}. object}
  \item{sem.data}{A data frame of variables used to generate the covariance matrix used in fitting the \code{\link{sem}}. object.}
  \item{...}{Additional arguments to \code{\link{sbchisq}}, which calculates several matrices used by browneChisq}
}

\details{The general quadratic form test statistic (Tqf) of Browne (1984, Proposition 4, Equation 2.20) for non-normally distributed data. It, too, is chi squared distributed.  If N is the sample size, s and sigma are vech of the observed and fitted covariance matrices respectively, then Tqf is:

\deqn{Tqf = N * (s-sigma)' * U * (s-sigma)}

Where U is calculated using D, the jacobian of the model, d(sigma)/d(theta), and Iw, the inverse of the fourth order moment matrix (i.e. the ADF weight matrix) such that

\deqn{U = Iw - (Iw * D * inv(D' * Iw * D) * D' * Iw)}.


}

\value{ An object of class \code{\link{adjchisq}}.}
  
\seealso{
	\code{\link{sem}}
	\code{\link{adjchisq}}
	\code{\link{sbchisq}}
}

\references{
Bentler, P.M. and Dudgeon, P.  1996.  Covariance structure analysis: Statistical practice, theory, and directions.  \emph{Annual Review of Psychology}.  47:563-592.

Browne, M.W.  1984.  Asymptotically distribution-free methods for the analysis of coviariance structures.  \emph{Br. J. Math. Stat. Psychol.}  37: 62-83

}