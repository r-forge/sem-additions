###
# test multivariate normality of several variables using a multivariate 
# shaprio wilks test
#
# Last updated 8/18/08
###

mnormtest<-function(a.frame){
	mt<-mshapiro.test(t(as.matrix(a.frame)))
	return(mt)
}

