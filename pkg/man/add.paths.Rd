\name{add.paths}
\alias{add.paths}
\title{Adds Paths From One Group of Variables to Another}
\description{ Automates the adding of directional paths to ram models, rather than coding it all by hand. }

\usage{add.paths(from, to, values=rep(NA, length(from)*length(to)))}
\arguments{
  \item{from}{A vector of predictor variables.}
  \item{to}{A vector of endogenous response variables.}
  \item{values}{Start values for the paths}
}

\details{Creates paths for all combinations of variables in one vector connecting to another.  Useful for large models with many connections from certain predictor variables.}


\seealso{
	\code{\link{sem}}
	\code{\link{specify.model}}
}

\examples{

#a piece of a larger model


model.kerch <- specify.model()
    Intelligence -> Grades,       gam51,    NA
    Siblings -> Grades,           gam52,    NA
    FatherEd -> Grades,           gam53,    NA
    FatherOcc -> Grades,          gam54,    NA
    Intelligence -> EducExp,      gam61,    NA
    Siblings -> EducExp,          gam62,    NA
    FatherEd -> EducExp,          gam63,    NA
    FatherOcc -> EducExp,         gam64,    NA
    
    
model.kerch2<-add.paths(c("Intelligence", "Siblings", "FatherEd", "FatherOcc"),
						c("Grades", "EducExp"))

##compare the two submodels - they are the same
##although with different names for coefficients
model.kerch
model.kerch2

}