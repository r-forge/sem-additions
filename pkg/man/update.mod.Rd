\name{update.mod}
\alias{update.mod}
\title{Modify an existing model}
\description{ Modify an existing model. }

\usage{
\method{update}{mod}(object, file="")}
\arguments{
  \item{object}{An object of class \code{ram}}  
  \item{file}{The (quoted) file from which to read the model specification changes, including the path to the file if it is not in the current directory. If "" (the default), then the specification is read from the standard input stream, and is terminated by a blank line.}  
}

\details{This function allows one to update a model by adding or removing paths without having to completely re-enter all of the code.  It therefore saves time and makes model debugging easier.  The format is similar to that found in \link{specify.model}.

Each line of the update specification consists of s series of terms separated by commas.  Lines begin with \code{add}, \code{delete}, or \code{replace}.  This defines what change is going to be invoked in the rest of this line.  Is a new line being added to the ram?  Is a particular line being deleted?  Or is some variable or coefficient being replaced with another?  

If the line begins with \code{add}, the rest of the entries in the line are a normal ram specification entry.

If the line begins with \code{delete}, the second entry is a coefficient, path, or variable name.  The third is one of three options -  \code{coefficient}, \code{path}, or \code{variable}.  The line in the model with the corresponding element is deleted, as in \link{delete.model.element}.

If the line begins with \code{replace}, the next two entries are a string of text that will be changed (e.g., a coefficient name), and the new text.  This can be useful, for example, when testing two models with the same form, but which use different variables for the same purpose.

}


\seealso{
	\code{\link{specify.model}}, \code{\link{delete.model.element}}, \code{\link{add.to.model}}, \code{\link{combine.models}}.
}


\examples{ 

#a quick demo
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

update(model.dhp)
add, newvar -> FEdASP, gam71, NA
add, newvar <-> newvar, theta5, NA
delete, FSES, variable
delete, theta1, variable #this will produce no change
delete, theta2, coefficient 
delete, RIQ -> RGenAsp, path
replace, FEdAsp, NewVar
  
   }