
###
# functions for robust SEs
###

sem_hessian<-function(w_mat, delta_mat){
	ret <- t(delta_mat) %*% w_mat %*% delta_mat
	return(ret)
}

robust_se<-function(sem.obj, adj.obj=NA, data.obj=NA, useFit=F){
	if(is.na(adj.obj[1]) && is.na(data.obj)) stop ("Need a data or sbchisq object")
	
	#get all of those matrices we'll need
	if(is.na(adj.obj[1])){ adj.obj<-sbchisq(sem.obj, data.obj, useFit=useFit)}
	
	ncases<-sem.obj$N
	
	#calculate the hessian
	hes<-sem_hessian(adj.obj$w_mat, adj.obj$p_deriv_mat)
	info_m<-ginv(hes)
	
	acov<- info_m %*% ( t(adj.obj$p_deriv_mat) %*% adj.obj$w_mat %*% adj.obj$w_adf %*% adj.obj$w_mat %*% adj.obj$p_deriv_mat) %*% info_m
	
	ret<-sqrt(diag(acov) / (ncases-1))
	names(ret)<-colnames(adj.obj$p_deriv_mat)
	return(ret)
}
