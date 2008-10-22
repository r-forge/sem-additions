\name{adjchisq}
\alias{adjchisq}
\alias{summary.adjchisq}
\alias{print.adjchisq}
\title{Adjusted Chi Squared Tests for Structural Equation Models}
\description{ Adjusted Chi Squared Tests for Structural Equation Models }

\usage{summary.adjchisq(adj.obj}{print.adjchisq(adj.obj)}
\arguments{
  \item{adj.obj}{An adjusted Chi Squre object from one of the several methods meant to deal with structural equations models with non-normal data.}
}

\details{Each test has its own peculiarities of calculation.  This general object holds all of the relevant information for uncorrected and corrected chi squared tests, depending on what type you have used.}

\value{Prints a matrix of the test.  Some of these may be null, depending on the test applied.
	\item{t}{The number of free parameters in the model}
	\item{p}{The total number of prameters in the model}
	\item{N}{The sample size of the model}
	\item{df}{The degrees of freedom for the fitted model}
	\item{d_c}{If non-null, a corrected degrees of freedom (for something like Browne's Chisq)}
	\item{chisq}{The unscaled chi squared value from the fitted model}
	\item{p.old}{The p value of for uncorrected chi squared test from the fitted model}
	\item{w_mat}{The normal theory weight matrix of the model}
	\item{w_adf}{The fourth order moment matrix for the data (i.e. the ADF weight matrix)}
	\item{p_deriv_mat}{The Jacobian matrix for the model - d(s)/d(theta) where theta are the parameters and s is vech(Sigma), the fitted covariance matrix}
	\item{res_u}{The residual weight matrix}
	\item{c}{The scaling or correction factor for the test}
	\item{chisq.scaled}{The adjusted Chi Squared statistic}
	\item{p}{The adjusted p value}
}
\seealso{
	\code{\link{sbchisq}}
	\code{\link{sbMeanChisq}}
	\code{\link{browneChisq}}
}


\references{
Bentler, P.M. and Dudgeon, P.  1996.  Covariance structure analysis: Statistical practice, theory, and directions.  \emph{Annual Review of Psychology}.  47:563-592.

Satorra, A. and Bentler, P. M.  1988.  Scaling corrections for chi-square statistics in covariance structure analysis. American Statistical Association.  \emph{Proceedings of the Business and Economic Statistics Section}.  308-313.

}