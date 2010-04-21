#last updated 4/20/2010

browneChisq<-function(sem.obj, sem.data, ...){
	adj.obj<-sbchisq(sem.obj, sem.data, ...)
	adj.obj$w_mat<-NA
	iw_adf<-ginv(adj.obj$w_adf)
	p_deriv_mat<-adj.obj$p_deriv_mat
	s<-vech(sem.obj$S)
	sigma<-vech(sem.obj$C)
	
	adj.obj$res_u<- iw_adf - (iw_adf%*%p_deriv_mat %*% solve(t(p_deriv_mat) %*% iw_adf %*% p_deriv_mat) %*% t(p_deriv_mat) %*% iw_adf)

	adj.obj$c<-NA
	adj.obj$chisq.scaled<- adj.obj$N * t(s-sigma) %*% adj.obj$res_u %*% (s-sigma)
	adj.obj$p<-1-pchisq(adj.obj$chisq.scaled, adj.obj$df)
	return(adj.obj)
}