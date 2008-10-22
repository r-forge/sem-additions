####
# methods to generate 
# various information criteria from
# sem or adjchisq objects
# as well as generate and AIC table
#
# Last updated 8/18/08
###


##
# general
#
aic<-function(object) UseMethod ("aic", object)
aicc<-function(object) UseMethod ("aicc", object)
caic<-function(object) UseMethod ("caic", object)
bic<-function(object) UseMethod ("bic", object)



##
# for sem objects
##
aic.sem<-function(object) {
	props<-sem.props(object)
	#AIC <- props$chisq - 2 * props$df
	AIC <- props$chisq + 2 * object$t
	return(AIC)
}

#small sample second order corrected aic
aicc.sem<-function(object) {
	props<-sem.props(object)
	AIC <- props$chisq + 2 * object$t*(object$t+1)/(object$N - object$t - 1)
	return(AIC)
}

#Bazdogan’s Consistent Akaike Information Criterion
caic.sem<-function(object) {
	props<-sem.props(object)
	caic <- props$chisq - props$df * (1 + log(props$N))
	return(caic)
}

bic.sem<-function(object) {
	props<-sem.props(object)
    BIC <- props$chisq - props$df * log(props$N)
	return(BIC)
}



###
# adjchisq cobjects
###

bic.adjchisq<-function(adj.obj){
	ret<-adj.obj$chisq.scaled - adj.obj$df*log(adj.obj$N)
	return(ret)
}

aic.adjchisq<-function(adj.obj){
	#ret<-adj.obj$chisq.scaled - 2*adj.obj$df
	ret<-adj.obj$chisq.scaled + 2*adj.obj$t
	return(ret)
}

#small sample second order corrected aic
aicc.adjchisq<-function(adj.obj){
	t<-adj.obj$t
	N<-adj.obj$N
	ret<-adj.obj$chisq.scaled + 2*t*(t+1)/(N-t-1)
	return(ret)
}


#Bazdogan’s Consistent Akaike Information Criterion
caic.adjchisq<-function(adj.obj){
	ret<-adj.obj$chisq.scaled - adj.obj$df*(1+log(adj.obj$N))
	return(ret)
}


###
# weights
###


aicW<-function(a.list, func=aicc){
	aiclist<-vector()
	aiclist<-sapply(a.list, function(x) eval(func(x)), simplify=T)
			
	delta.i<-aiclist-min(aiclist)
	aicw<-exp(delta.i*-0.5)/sum(exp(delta.i*-0.5))
	return.matrix<-matrix(c(aiclist,delta.i, aicw), ncol=3)
	colnames(return.matrix)<-c("IC","delta.i", "weight")
	rownames(return.matrix)<-1:length(return.matrix[,1])
	return(return.matrix)
}
