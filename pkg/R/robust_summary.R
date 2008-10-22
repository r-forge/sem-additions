robust_summary<-function(sem.obj, adj.obj=NA, data.obj=NA, useFit=F){
	if(is.na(adj.obj[1]) && is.na(data.obj)) stop ("Need a data or adjchisq object")
	
	#get all of those matrices we'll need
	if(is.na(adj.obj[1])){ adj.obj<-sbchisq(sem.obj, data.obj, useFit=useFit)}
	

	ses<-robust_se(sem.obj, adj.obj=adj.obj)
	se<-rep(NA, length(sem.obj$coef))
	
	for (i in 1:length(sem.obj$ram[,1])){
		if(sem.obj$ram[i,4] > 0){
			se[i]<-ses[i]			
			}
		
		}
	z <- sem.obj$coef/se
	p<-2*(1 - pnorm(abs(z)))
	#print the chisquare results
	print(adj.obj)
	cat(paste("\n", "AICc", " = ", aicc.adjchisq(adj.obj), "\n", sep=""))
	cat(paste("BIC", " = ", bic.adjchisq(adj.obj), "\n", sep=""))
	coef.mat<-matrix(c( sem.obj$coef, se, z, p), ncol=4)
	rownames(coef.mat)<-names(sem.obj$coef)
	colnames(coef.mat)<-c("Estimate", "SE", "z value", "Pr(>|z|)")
	print(coef.mat)
	
}