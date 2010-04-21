
###
# functions for robust SEs
###

sem_hessian<-function(w_mat, delta_mat){
	ret <- t(delta_mat) %*% w_mat %*% delta_mat
	return(ret)
}

robust_se<-function (sem.obj, adj.obj = NA, data.obj = NA, useFit = F, useGinv=F) 
{
    if (is.na(adj.obj[1]) && is.na(data.obj)) 
        stop("Need a data or sbchisq object")
    if (is.na(adj.obj[1])) {
        adj.obj <- sbchisq(sem.obj, data.obj, useFit = useFit)
    }
    ncases <- sem.obj$N
    hes <- sem_hessian(adj.obj$w_mat, adj.obj$p_deriv_mat)
    #having issues with this inversion in certain models
    #until it is locked down, ginv provides a reasonable
    #approximation
    info_m <- try(solve(hes), silent=T)
    if(class(info_m)=="try-error" & useGinv==T) {
    		info_m <-ginv(hes)
    		ginvFlag<-T
    }
    acov <- info_m %*% (t(adj.obj$p_deriv_mat) %*% adj.obj$w_mat %*% 
        adj.obj$w_adf %*% adj.obj$w_mat %*% adj.obj$p_deriv_mat) %*% 
        info_m
    ret <- sqrt(diag(acov)/(ncases - 1))
    names(ret) <- colnames(adj.obj$p_deriv_mat) #not sure if this is needed
    return(ret)
    
    #have dealt with this in delta_mat
    #return(ret[-which(sem.obj$ram[,4]==0)])
}

