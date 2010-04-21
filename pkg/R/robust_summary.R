#last updated 4/20/2010
robust_summary<-function (sem.obj, adj.obj = NA, data.obj = NA, useFit = F, useGinv=F) 
{
    if (is.na(adj.obj[1]) && is.na(data.obj)) 
        stop("Need a data or adjchisq object")
    if (is.na(adj.obj[1])) {
        adj.obj <- sbchisq(sem.obj, data.obj, useFit = useFit, useGinv= useGinv)
    }
    ses <- robust_se(sem.obj, adj.obj = adj.obj, useGinv=adj.obj$ginvFlag)
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

