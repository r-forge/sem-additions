\name{mnormtest}
\alias{mnormtest}
\title{Test for Multivariate Normality}
\description{Implements Shapiro-Wilk's test for multivariate normality for matrices and data frames}
\usage{mnormtest(a.frame)}

\argument{
	\item{a.frame}{A data frame containing only the data of interest}
}

\details{Provides an easy interface for assessing the multivariate normality of a single data frame using the Multivariate Shapiro-Wilk's test with \code{\link{mshapiro.test}}
}

\value{}

\seealso{
	\code{\link{mshapiro.test}}
}


\references{
Bentler, P.M. and Dudgeon, P.  1996.  Covariance structure analysis: Statistical practice, theory, and directions.  \emph{Annual Review of Psychology}.  47:563-592.


}