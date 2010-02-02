##
#Calcute the Satorra-Bentler Chi Square Index
#Adapted from SPSS code by Peter Bentler
#Returns a number of matrices also useful to other functions
##
sbchisq<-function(sem.obj, sem.data, adj=1e-04, useFit=F){
	props<-sem.props(sem.obj)
	sem.prop <- props$chisq
	chisq<-props$chisq
	df<-props$df
	
	#calculate the ADF weight matrix
	w_adf<-adf.wmat(sem.data)
	
	#get the GLS weight matrix	
	w_mat<-ml.wmat(sem.obj, useFit=useFit)
	
	#get the delta matrix, the partial derivative of changes in covariances
	#for changes in parameter values
	p_deriv_mat<- delta_matrix(sem.obj)
	
	#calculate the LS residual weight matrix
	res_u<- w_mat - (w_mat%*%p_deriv_mat %*% solve(t(p_deriv_mat) %*% w_mat %*% p_deriv_mat) %*% t(p_deriv_mat) %*% w_mat)

	#compute scaling statistic
	ug<-res_u %*% w_adf
	scale_stat<-tr(ug)/df
	
	#compute 
	chisq.scaled<-chisq/scale_stat
	p.old<-1-pchisq(chisq, df)
	p<-1-pchisq(chisq.scaled, df)
	ret<-list( chisq = chisq,
				t  = sem.obj$t,
				df = df,
				p.old = p.old,
				c = scale_stat,
				chisq.scaled = chisq.scaled,
				p = p,
				w_mat = w_mat,
				p_deriv_mat = p_deriv_mat,
				w_adf = w_adf,
				res_u = res_u,
				N = length(sem.data[,1]))
	class(ret)<-"adjchisq"

	return(ret)
}
