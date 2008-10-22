##	
#Ktrans - Creates the transition matrix [kp] which can be
# used for selecting the non-duplicated elements of a
# symmetric matrix & placing them in a vector
# Adapted from Stas Kolenikov's MATA code for a duplication matrix by JEB
#
# Last Updated 9/02/08
##

Ktrans<-function(num.vars){
	pstar<-num.vars*(num.vars+1)/2
	Ipstar<-ident.mat(pstar)
	D.mat<-c()
	for (k in 1:pstar) D.mat<-c(D.mat, vec(invvech(Ipstar[,k])))
	D.mat<-matrix(D.mat, ncol=pstar)
	return(D.mat)
}

