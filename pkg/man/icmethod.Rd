\name{icmethods}
\alias{aic}
\alias{aicc}
\alias{bic}
\alias{caic}
\title{Information Criterion Methods for Structural Equation Models}
\description{Information Criteria for Structural Equation Models}
\usage{
aic(object)
aicc(object)
bic(object)
}

\arguments{
	\item{object}{An object of class \code{\link{sem}}, \code{\link{adjchisq}}, or \code{\link{bsboot}}
}
}

\details{Calculates information criterion values for structural equation models.  Methods will also handle alternate objects that contain chi squared indexes for structural equation models.  The formulas for each are as follows, with X as the chi squared value, df as the model degrees of freedom, t is the number of free parameters, and N is the sample size:

Akaike's Information Criterion (AIC) = \eqn{X + 2*t}

Small Sample Akaike's Information Criterion (AICc) = \eqn{X + 2*t*(t+1)/(N-t-1) }

Schwartz-Bayes Information Criterion (BIC) = \eqn{X - df * log(N)}

Bazdogan's Consistent Akaike Information Criterion (CAIC) = \eqn{X - df*log(1+N}

}


\seealso{
	\code{\link{sem}}
	\code{\link{sbchisq}}
	\code{\link{adjchisq}}
	\code{\link{bsboot}}
	\code{\link{aicW}}
}

