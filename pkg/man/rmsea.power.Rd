\name{rmsea.power}
\alias{rmsea.power}
\title{RMSEA Power Calculation}

\description{Calculate the power for testing RMSEA based on MacCallum, Browne
and Sugawara (1996)}

\usage{rmsea.power(rmsea.ha, df, n, rmsea.h0=0.05, alpha=0.05)}

\argument{
 \item{rmsea.ha}{The RMSEA of the alternate hypothesis}
 \item{df}{The degrees of freedom of the sem model}
 \item{n}{Sample size}
 \item{rmsea.h0}{The RMSEA of the Null hypothesis. MacCallum et al. (1996)
 recommend this set to .05 as typically models are not expected to fit perfectly
 and .05 is an accepted critical value in the literature for a good model.}
 \item{alpha}{The type I error probability}
}

\detail{MacCallum et al. (1996) provide power and sample size calculations for
the RMSEA fit statistic for covariance structures, based on the Chi square
distribution. This function is based on their SAS code provided in the paper.
MacCallum et al. (1996) suggest three typical and meaningful tests:

\itemize{
\item{Test of close fit: rmsea.ha = 0.08, rmsea.h0 = 0.05}
\item{Test of not-close fit: rmsea.ha = 0.01, rmsea.h0 = 0.05}
\item{Test of exact fit: rmsea.ha = 0.05, rmsea.h0 = 0.00}
}
}

\value{The power of the test.}

\author{Joerg Evermann}\email{jevermann@mun.ca}

\sealso{\code{\link{sem}}}

\references{
MacCallum, R.C.; Browne, M.W. and Sugawara, H.M. 1996. Power analysis and
determination of sample size for covariance structure modeling.
\emph{Psychological Methods}. 1:130-149.
}

