##
#Delta macro - Computes the partial derivatives of the model parameters with
#  respect to the non-duplicated fitted covariances
# Adapted from SPSS code by Peter Bentler
#                 
# Changelog 
# 9/1/08 - fixed matrix selection bug
##

delta_matrix<-function(sem.object, adj=1e-04){
	p.star<-sem.object$n*(sem.object$n+1)/2
	nparams<-length(sem.object$ram[,1])
	delta.mat<-matrix(0,nparams, p.star)
	rownames(delta.mat)<-rep(NA, nparams)
	colnames(delta.mat)<-vech(matrix.names(sem.object$C))
	vars<-sem.object$var.names
	
	C.vect<-vech(sem.object$C)
	J<-sem.object$J
	m<-sem.object$m
	
	#iterate through the ram
	for (i in 1:nparams){
	  A<-sem.object$A
 	  P<-sem.object$P
  	  if(sem.object$ram[i,1]==1){
	  	  A[sem.object$ram[i,2],sem.object$ram[i,3]]<-
	  	  	A[sem.object$ram[i,2],sem.object$ram[i,3]]+adj
	  }else{
	  	  P[sem.object$ram[i,2],sem.object$ram[i,3]]<-
	  	  	P[sem.object$ram[i,2],sem.object$ram[i,3]]+adj
	  }	
	  	
	  I.Ainv <- solve(diag(m) - A)
	  	
	  #calculate fitted covarianve matrix from RAM form
	  #C=  J(Im A) 1P[(Im A) 1]J using the RAM formulation
       C <- J %*% I.Ainv %*% P %*% t(I.Ainv) %*% t(J)
       C.vech<-vech(C)

       #get difference with model cov matrix / adj
       #linearize, and put into delta matrix
       delta.mat[i,]<-(C.vech - C.vect)/adj
       rownames(delta.mat)[i] <-rownames(sem.object$ram)[i]
       if(rownames(delta.mat)[i] ==""){
       	rownames(delta.mat)[i] <-
       	  paste(vars[sem.object$ram[i,3]],
       	  		vars[sem.object$ram[i,2]],
       	  		sep="-")
       }
	  	
	  	
	}
	
	#whoops!  reversed rows and cols - here's a quick fix
	delta.mat<-t(delta.mat)

	return(delta.mat)
	
}