\name{anova.adjchisq}
\alias{anova.adjchisq}
\title{Adjusted Chi Squared Tests for Structural Equation Models}
\description{ Adjusted Chi Squared Tests for Structural Equation Models \link{sbchisq}}

\usage{anova.adjchisq(adjobj0, adjobj1)

}

\arguments{
  \item{adjobj0}{An adjusted Satorra-Bentler Chi Squre object from \link{sbchisq}}
  \item{adjobj1}{An adjusted Satorra-Bentler Chi Squre object from \link{sbchisq}}
}

\details{Compares two nested models that have been corrected for non-normality of the data}

\value{Prints a matrix of the test similar to  \link{anova}.  Note, due to scaling, the Chi Square evaluated will not be the same as the difference between the two model Chi Square values.

The corrected chi square difference test is calculated as follows - As one model is nested within the other, let's call m0 the nested model and m1 the more general model.  DF = df0 - df1.  

To calculate the chi square difference between these two models, we need to look at the correction factors - c0 and c1 - and calculate a correction factor for the test itself, cd.

\deqn{cd = (df0* chisq0 - df1 * chisq1)/(df0 - df1)}

where chisq is the uncorrected chi square value for each model.  The Chi Square statistic, TRd, is then defined as follows:

\deqn{TRd = (chisq0 - chisq1)/cd}

If the result is significant, the more complex model is favored.  If not, the two are equivalent in fit.

}

\seealso{
	\code{\link{sbchisq}}
	\code{\link{anova}}
	\code{\link{http://www.statmodel.com/chidiff.shtml}}
}


\references{
Satorra, A. 2000. Scaled and adjusted restricted tests in multi-sample analysis of moment structures. In Heijmans, R.D.H., Pollock, D.S.G. & Satorra, A. (eds.), Innovations in multivariate statistical analysis. A Festschrift for Heinz Neudecker (pp.233-247). London: Kluwer Academic Publishers.

\code{\link{http://www.statmodel.com/chidiff.shtml}}

}