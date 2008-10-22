sbMeanChisq<-function(sem.obj, sem.data, ...){
	adj.obj<-sbchisq(sem.obj, sem.data, ...)
	u<-adj.obj$res_u
	g<-adj.obj$w_adf
	adj.obj$d_c<-(tr(u%*%g))^2 / tr((u%*%g)^2)
	adj.obj$c<-tr(u%*%g)/adj.obj$d_c	
	adj.obj$chisq.scaled<-adj.obj$chisq/adj.obj$c
	adj.obj$p<-1-pchisq(adj.obj$chisq.scaled, adj.obj$d_c)
	return(adj.obj)
}
