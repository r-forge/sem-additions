\name{aicW}
\alias{aicW}
\title{Information Criterion Weights}
\description{Calculates the information criterion weight table for a number of models}
\usage{aicW(a.list, func=aicc)}

\arguments{
	\item{a.list}{a list of objects which can have information criterion values returned}
	\item{func}{a function for generating information criterion values.  Defaults to \code{\link{aicc}}, the small sample corrected Akaike's Information Criterion.}
}

\details{Calculates the information criterion of interest for a list of objects.  Then creates an information criterion weight table.
}


\seealso{
	\code{\link{sem}}
	\code{\link{adjchisq}}
	\code{\link{bsboot}}
	\code{\link{aicc}}
	\code{\link{aic}}
	\code{\link{bic}}
	\code{\link{caic}}
}

\references{
Burnham, K. P., and D. R. Anderson. 2002. Model selection and multimodel inference: a practical information-theoretic approach. 2nd Edition. Springer-Verlag, New York, New York, USA. 488 pp.
}