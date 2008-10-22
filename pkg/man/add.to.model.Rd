\name{add.to.model}
\alias{add.to.model}
\title{Add new paths to a model}
\description{ Adds new paths and covariances to a ram model object. }

\usage{add.to.model(old.model, file="", ...)}
\arguments{
  \item{old.model}{An object of class \code{ram} to which paths need to be added}  
  \item{file}{The (quoted) file from which to read the model specification, including the path to the file if it is not in the current directory. If "" (the default), then the specification is read from the standard input stream, and is terminated by a blank line.}  
  \item{...}{Additional arguments to \code{\link{specify.model}.}}
}

\details{This function allows one to update a model with new paths, or to create an alternate model with more paths than a previous one without having to completely re-enter all of the code.  It therefore saves time and makes model debugging easier.  The format is exactly the same as that found in \link{specify.model}..}


\seealso{
	\code{\link{specify.model}}.
}


\examples{ 

    ## Same model as in \code{\link{specify.model}}.
    ## broken into two pieces

model.dhp <- specify.model()
    RParAsp  -> RGenAsp, gam11,  NA
    RIQ      -> RGenAsp, gam12,  NA
    RSES     -> RGenAsp, gam13,  NA
    FSES     -> RGenAsp, gam14,  NA
    RSES     -> FGenAsp, gam23,  NA
    FSES     -> FGenAsp, gam24,  NA
    FIQ      -> FGenAsp, gam25,  NA
    FParAsp  -> FGenAsp, gam26,  NA
    FGenAsp  -> RGenAsp, beta12, NA
    RGenAsp  -> FGenAsp, beta21, NA


model.dhp<-add.to.model(model.dhp)
	RGenAsp  -> ROccAsp,  NA,     1
    RGenAsp  -> REdAsp,  lam21,  NA
    FGenAsp  -> FOccAsp,  NA,     1
    FGenAsp  -> FEdAsp,  lam42,  NA
    RGenAsp <-> RGenAsp, ps11,   NA
    FGenAsp <-> FGenAsp, ps22,   NA
    RGenAsp <-> FGenAsp, ps12,   NA
    ROccAsp <-> ROccAsp, theta1, NA
    REdAsp  <-> REdAsp,  theta2, NA
    FOccAsp <-> FOccAsp, theta3, NA
    FEdAsp  <-> FEdAsp,  theta4, NA

model.dhp
    ## End(Not run)
    
   }