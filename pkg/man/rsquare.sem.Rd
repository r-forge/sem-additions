\name{rsquare.sem}
\alias{rsquare.sem}
\title{R Squared For SEM}
\description{Calculate R Squared Values of Structural Equation Models}

\usage{rsquare.sem(sem.obj)}

\arguments{
  \item{sem.obj}{a fitted sem object} 
 }

\details{ Calculated the R Squared for observed endogenous variables in a structural equation model, as well as several other useful error summary statistics.

R Squared values are calculated as 

\deqn{R^{2} = 1-\frac{estimated \, variance}{observed \, variance}}{R^2 = 1- (estimated variance)/(observed variance)}

Standardized error coefficients are then calculated as \eqn{\sqrt[ ]{ 1 - R^{2}}}{sqrt(1-R^2)}.

}

\seealso{
  \code{\link{sem}}.
}



