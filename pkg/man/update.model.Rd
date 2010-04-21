\name{update.model}
\alias{update.model}
\alias{model.to.ram}
\alias{ram.to.model}
\title{Create and modify a set of formulas to specify a SEM model}
\description{ Methods for dealing with formulas instead of ram objects for sem.  Made to make sem and lavaan cross-compatible. }

\usage{
\method{update}{model}(old.model, changes)

model.to.ram(a.model)

ram.to.model(ram.obj, latents=NA)
}
\arguments{
  \item{old.model}{A series of formulas in a text string separated by line breaks.}  
  \item{changes}{A series of modifications to the formula separated by line breaks in the style of \code{update.formula}.}  

  \item{a.model}{A series of formulas in a text string separated by line breaks.}  

  \item{ram.obj}{A \link{ram} object from \link{specify.model} or related methods.}  
  \item{latents}{A vector of latent variable names.}  
}

\details{The upcoming lavaan package implements a scheme for specifying Structural Equation Models using formulae rather than a ram matrix.  Formulas are enclosed in a single quoted string and are separated by line breaks.  Comments are allowed throughout.  These methods allow one to use such an object in the sem package.  One may also freely convert back and forth between the two formats, and update lavaan model objects.

The syntax of the formulae in lavaan model objects is as follows

\code{y~x+z}
is a path from both x and y to z.

\code{X =~ x1+x2}
X is a latent variable that is measured by the variables x1 and x2.

\code{y ~~ x+z}
y covaries with x and z.

\code{y~3*x+z}
In this example, the coefficient for x is fixed at 3.
}

\value{
  \code{model.to.ram} returns an object of class \code{ram}.
 }

\seealso{
	\code{\link{specify.model}}, \code{\link{update.mod}}.
}


\examples{ 

#create a model and turn it into a ram
a.model<-'y~x+z
		  x~3*z
		  z=~a+b+c
		  y~~c
'

cat(a.model)
model.to.ram(a.model)

#create a ram and turn it into a model
a.ram<-specify.model()
a->b, ab, NA
b->c, bc, NA
d->c, NA, 3
a<->c, ac.cov, NA

cat(ram.to.model(a.ram, latents="b"))
  
   }