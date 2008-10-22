###
#Gets weight matrix from Fml
#using calculation from Satorra and Bentler 1988
#W = 2^-1 * Dp' * (An^-1 kronecker An^-1) * Dp
#where Dp is a dupplication matrix with order p
#and An is the fitted covariance matrix or the sample cov matrix
#
# Last Updated 8/18/08
###

ml.wmat<-function(sem.obj, useFit=F){
	p<-length(rownames(sem.obj$C))
	if(useFit){An<-sem.obj$C} else {An<-sem.obj$S}
	Dp<-Ktrans(p)
	An.inv<-ginv(An)
	w_mat<-2^-1 * t(Dp) %*% kronecker(An.inv, An.inv) %*% Dp
	rownames(w_mat)<-vech(matrix.names(sem.obj$C))
	colnames(w_mat)<-rownames(w_mat)
	return(w_mat)
}