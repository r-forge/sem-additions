robust_summary<-function (sem.obj, adj.obj = NA, data.obj = NA, useFit = F) 
{
    if (is.na(adj.obj[1]) && is.na(data.obj)) 
        stop("Need a data or adjchisq object")
    if (is.na(adj.obj[1])) {
        adj.obj <- sbchisq(sem.obj, data.obj, useFit = useFit)
    }
    ses <- robust_se(sem.obj, adj.obj = adj.obj)
    se <- rep(NA, length(ses))
	index<-0
	for (i in 1:length(sem.obj$ram[, 1])) {
        if (sem.obj$ram[i, 4] > 0) {cat(rownames(sem.obj$ram[i]))
        	   index<-index+1
            se[index] <- ses[index]
        }
    }
    
    z <- sem.obj$coef/se
    p <- 2 * (1 - pnorm(abs(z)))
    print(adj.obj)
    cat(paste("\n", "AICc", " = ", aicc.adjchisq(adj.obj), "\n", 
        sep = ""))
    cat(paste("BIC", " = ", bic.adjchisq(adj.obj), "\n", sep = ""))
    coef.mat <- matrix(c(sem.obj$coef, se, z, p), ncol = 4)
    rownames(coef.mat) <- names(sem.obj$coef)
    colnames(coef.mat) <- c("Estimate", "SE", "z value", "Pr(>|z|)")
    print(coef.mat)
}

add.paths<-function(from, to, values=rep(NA, length(from)*length(to))){
	newram<-matrix(rep(NA, 3*length(from)*length(to)), ncol=3)
	par.index<-0
	for (i in 1:length(to)){
	  for (j in 1:length(from)){
		par.index<-par.index+1
		newram[par.index,1]<-paste(from[j], "->", to[i], sep="")
		newram[par.index,2]<-paste(from[j], ".", to[i], sep="")
		newram[par.index,3]<-values[par.index]
		}
	}
	
	class(newram)<-"mod"
	return(newram)
}