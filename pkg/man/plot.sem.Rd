\name{plot.sem}
\alias{plot.sem}
\alias{plot.ram}

\title{Plot SEM and RAM Objects}
\description{ A function for plotting fitted structural equation models }

\usage{
plot.sem(sem.object, add.error=F, sig.col="black", ns.col="darkgrey",  ...)

plot.ram(ram.object, ...)
}

\arguments{
  \item{sem.object}{A fitted sem object}
  \item{ram.object}{A ram object generated by \code{\link{specify.model}}.}
  \item{add.error}{Should error and covariance paths be shown?  Defaults to False}
  \item{sig.col}{The color of paths that are significant at p<=0.05}
  \item{nl.col}{The color of paths that are not significant at p<0.05}
  \item{...}{Additional arguments to gplot.}
}

\details{In order to visualize a fittem sem object or an unfitted ram object, this function extacts the ram matrix and turns it into a graph from \code{\link{sna}}..  It them plots the graph using \code{\link{gplot}}., coloring paths as significant or not significant.}


\seealso{
	\code{\link{sna}}. 
	\code{\link{gplot}}.
	\code{\link{sem}}.
	\code{\link{specify.model}}.
}
