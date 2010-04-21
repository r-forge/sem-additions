##
#Delta macro - Computes the partial derivatives of the model parameters with
#  respect to the non-duplicated fitted covariances
# Adapted from SPSS code by Peter Bentler
#                 
# Changelog
# 3/5/10 - fixed bug of not adjusting for correlated disturbance on both parts of P matrix 
# 2/8/10 - fixed bug of using fixed params
# 9/1/08 - fixed matrix selection bug
##

delta_matrix<-function(sem.object, adj=1e-04){
	p.star<-sem.object$n*(sem.object$n+1)/2
	#nparams<-length(sem.object$ram[,1])
	#only use free parameters
	par.idx<-which(sem.object$ram[,"parameter"]>0)
	nparams<-length(par.idx)
	delta.mat<-matrix(0,nparams, p.star)
	rownames(delta.mat)<-rep(NA, nparams)
	colnames(delta.mat)<-vech(matrix.names(sem.object$C))
	vars<-sem.object$var.names
	
	C.vect<-vech(sem.object$C)
	J<-sem.object$J
	m<-sem.object$m
	
  #iterate through the ram
  #but only look at free params
  for (i in 1:nparams){
  	A<-sem.object$A
  	P<-sem.object$P
  	from<-sem.object$ram[par.idx[i],2]
  	to<-sem.object$ram[par.idx[i],3]
  	path_type <-sem.object$ram[par.idx[i],1]
  	if(path_type==1){
  	  A[from, to] <-
  	     A[from, to] + adj
	}else{
	   P[from, to]<- P[from,to]+adj


	   #symmetric P matrix 
	   P[to, from]<- P[to,from]
	}
	
	I.Ainv <- solve(diag(m) - A)
	  	
	  #calculate fitted covarianve matrix from RAM form
	  #C=  J(Im A) 1P[(Im A) 1]J using the RAM formulation
       C <- J %*% I.Ainv %*% P %*% t(I.Ainv) %*% t(J)
       C.vech<-vech(C)

       #get difference with model cov matrix / adj
       #linearize, and put into delta matrix
       delta.mat[i,]<-(C.vech - C.vect)/adj
       rownames(delta.mat)[i] <-rownames(sem.object$ram)[par.idx[i]]
       if(rownames(delta.mat)[i] ==""){
       	rownames(delta.mat)[i] <-
       	  paste(vars[sem.object$ram[par.idx[i],3]],
       	  		vars[sem.object$ram[par.idx[i],2]],
       	  		sep="-")
       }
	  	
	  	
	}
	
	#whoops!  reversed rows and cols - here's a quick fix
	delta.mat<-t(delta.mat)

	return(delta.mat)
	
}