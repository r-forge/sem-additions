##
#returns the r^2 for endogenous variables and error path coefficients
#
# Last updated 8/18/08
##

rsquare.sem<-function (sem.obj){
	output.info<-list()
	
	for (index in 1:length(sem.obj$S[,1])){
		var.name<-sem.obj$var.names[index]
		obs.var<-sem.obj$S[index,index]
		est.var<-sem.obj$P[index,index]
		r.sq<-1-(est.var/obs.var)
		std.error.coef<-sqrt(1-r.sq)
		
		#if it's 0, or close to (due to rounding error), it's exogenous
		if(r.sq > 1e-10){
		output.info<-c(output.info, var.name, obs.var, est.var, r.sq, std.error.coef)
		}
		
	}
	
	output.matrix<-matrix(output.info, ncol=5, byrow=TRUE)
	colnames(output.matrix)<-c("Variable", "Observed.Variance", "Estimated.Variance", "R^2", "Standardized.Error.Coefficient")
	output.matrix
}
