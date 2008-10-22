###
# Functions for obtaining and manipulating the Bollen-Stine Bootstrap
# for Structural Equation Modelling
#
# Jarrett Byrnes, byrnes@msi.ucsb.edu
# last updated 8/18/08
###

#centers a data frame around the mean of each value
centmean<-function(raw_data){
	n<-length(raw_data[,1])	
	i1<-rep(1, n)
	xbar<-apply(raw_data, 2, sum)/n
	z<-raw_data-kronecker(xbar, i1)
	return(z)
}

#transforms a data set for a bollen-stine bootstrap using
#the cholsky decomposition of the observed and fitted
#covariance matrices
#from Bollen and Stine; ModelsBootstrapping Goodness-of-Fit Measures in
# Structural Equation Models; 1992; 21; 205 Sociological Methods Research 

get.bs.data<-function(model.data.frame, sem.object){
	cov.mat<-sem.object$S
	model.cov.mat<-sem.object$C

	#get a centered data matrix
	data.mat<-as.matrix(centmean(model.data.frame))

	chol.data<-data.mat %*% ginv(chol(cov.mat)) %*% chol(model.cov.mat)
	chol.data<-data.frame(chol.data)
	return(chol.data)
}


#gets a covariance matrix for data
#by sampling it with replacement
make.boot.var<-function(bs.data){
	require(boot)
	n<-length(bs.data[,1])
	new.indices<-sample(n, n, replace=T)
	new.var<-var(bs.data[new.indices,])
	return(new.var)
}

bs.boot.chisq<-function(model, model.data.frame, R=2000, maxiter=500, steptol=1e-06, t=10, ...){

	#get necessary properties from original model
	N<-length(model.data.frame[,1])
	orig.cov<-var(model.data.frame)
	print(paste("calculating original sem..."))
	orig.sem<-sem(model, orig.cov, N=N, maxiter=500, steptol=1e-06, ...)
	n.fix<-orig.sem$n.fix	
	n<-orig.sem$n
	t<-orig.sem$t
	df <- n*(n + 1)/2 - t - n.fix*(n.fix + 1)/2

	#create modified data for the bootstrapping, and a 
	#chisq vector for each bootstrap replicate
    	bs.data<-get.bs.data(model.data.frame, orig.sem)
	chisq<-rep(NA, R)
	for (i in 1:R){
		print(paste("bs ",i,sep=""))
		mysteptol<-steptol
		upstep<-F

		#fit, and, if there are errors, try and make corrections
		#if nothing corrects it, there will be an NA for this round
		for (j in 1:t){
			cov.mat<-make.boot.var(bs.data)

			if (upstep) mysteptol=mysteptol*10

			boot.sem<-sem(model, cov.mat, N=N, ...)
			if(inherits(boot.sem, "try-error")){
				print(paste("t", t+1, sep=""))
				if(convergence==3) upstep<-T
				if(convergence==4) maxiter<-maxiter+100
			}
			else{
				chisq[i] <- boot.sem$criterion * (N - (!boot.sem$raw))
				break()
			}
		}
	}
		
	bs.boot.obj<-list()
	bs.boot.obj$df<-df
	bs.boot.obj$R<-R
	bs.boot.obj$N<-N
	bs.boot.obj$t<-t
	bs.boot.obj$chisq.vec<-chisq
	class(bs.boot.obj)<-"bsboot"
	return(bs.boot.obj)
	
}
	
summary.bsboot<-function(object){
	mean.chisq<-mean(object$chisq.vec, na.rm=T)
	se.chisq<-se(object$chisq.vec)
	var.chisq<-var(object$chisq.vec, na.rm=T)
	p<-1-pchisq(mean.chisq, object$df)
	ret.mat<-matrix(c(mean.chisq, var.chisq, se.chisq, object$R, object$df, p), nrow=1)
	rownames(ret.mat)<-""
	colnames(ret.mat)<-c("Mean Chisq", "Variance", "Standard Error", "# Bootstraps", "DF", "Mean P")
	return(ret.mat)
}

#plots the bootstrapped chi square distribution and the actual chi square 
#distribution for the df of the model
plot.bsboot<-function(object, chisq.scale=0.1, 
			main="", xlab="", ylab="frequency",...){
	xmax<-max(object$chisq.vec)+10
	if(qchisq(0.999, object$df)>xmax) xmax<-qchisq(0.999, object$df)+10

	a<-seq(0, xmax, chisq.scale)
	ch<-dchisq(a, object$df)
	ymax<-max(ch)

	plot(density(object$chisq.vec), xlim=c(0, xmax), ylim=c(0,ymax),lwd=2, col="red", main=main, xlab=xlab, ylab=ylab, ...)
	matplot(a, ch, add=T, lwd=2, col="black", type="l")
	legend("topright",c("Chi Square Distribution", "Bootstrapped Distribution"), col=c("black", "red"), lwd="1")
}
	
aic.bsboot<-function(object){
	chisq<-mean(object$chisq.vec, na.rm=T)
	AIC<-chisq+2*object$t	
	return(AIC)
}

aicc.bsboot<-function(object){
	chisq<-mean(object$chisq.vec, na.rm=T)
	AICc<-chisq+2 * object$t*(object$t+1)/(object$N - object$t - 1)
	return(AICc)
}

bic.bsboot<-function(object) {
    BIC <- mean(object$chisq.vec, na.rm=T) - object$df * log(object$N)
	return(BIC)
}

caic.bsboot<-function(object) {
	props<-sem.props(object)
	AICc <- mean(object$chisq.vec, na.rm=T) - object$df * (1 + log(object$N))
	return(AICc)
}