\name{find.exogenous}
\alias{find.exogenous}
\title{Display Exogenous Variables}
\description{ Explores a ram object and displays which variables are exogenous. }

\usage{find.exogenous(model)}

\arguments{
  \item{model}{a ram object created by \code{\link{specify.model}}.}
}

\details{Scans through a model, and returns the names of variables that do not have a path pointing to them.}


\seealso{
	\code{\link{specify.model}}.
	\code{\link{sem}}.
}
