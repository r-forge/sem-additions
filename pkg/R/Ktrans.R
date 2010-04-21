##	
#Ktrans - Creates the transition matrix [kp] which can be
# used for selecting the non-duplicated elements of a
# symmetric matrix & placing them in a vector
# Adapted from Stas Kolenikov's MATA code for a duplication matrix by JEB
#
# 2/8/20 - realized matrixcalc package has a MUCH faster way of doing this
# Last Updated 2/08/10
##

Ktrans<-function(num.vars){
	#pstar<-num.vars*(num.vars+1)/2
	#Ipstar<-ident.mat(pstar)
	#D.mat<-c()
	#for (k in 1:pstar) D.mat<-c(D.mat, vec(invvech(Ipstar[,k])))
	#D.mat<-matrix(D.mat, ncol=pstar)
	require(matrixcalc)
	D.mat<-duplication.matrix(num.vars)
	return(D.mat)
}

