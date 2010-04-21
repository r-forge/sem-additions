##
#Calcute the Satorra-Bentler Chi Square Index
#Adapted from SPSS code by Peter Bentler
#Returns a number of matrices also useful to other functions
#last updated 4/20/2010
##
sbchisq<-function (sem.obj, sem.data, adj = 1e-04, useFit = F, useGinv=F) 
{
    props <- sem.props(sem.obj)
    sem.prop <- props$chisq
    chisq <- props$chisq
    df <- props$df
    w_adf <- adf.wmat(sem.data)
    w_mat <- ml.wmat(sem.obj, useFit = useFit)
    p_deriv_mat <- delta_matrix(sem.obj)
   
    #having issues with this inversion in certain models
    #until it is locked down, ginv provides a reasonable
    #approximation
    ginvFlag<-F
    invMat<-try(solve(t(p_deriv_mat) %*% w_mat %*% p_deriv_mat), silent=T)
    if(class(invMat)=="try-error" & useGinv==T) {
    		invMat<-ginv(t(p_deriv_mat) %*% w_mat %*% p_deriv_mat)
    		ginvFlag<-T
    }
    res_u <- w_mat - (w_mat %*% p_deriv_mat %*% invMat %*% t(p_deriv_mat) %*% w_mat)
    
    ug <- res_u %*% w_adf
    scale_stat<-sum(diag(ug))/df
    chisq.scaled <- chisq/scale_stat
    p.old <- 1 - pchisq(chisq, df)
    p <- 1 - pchisq(chisq.scaled, df)
    
    ret <- list(chisq = chisq, t = sem.obj$t, df = df, p.old = p.old, 
        c = scale_stat, chisq.scaled = chisq.scaled, p = p, w_mat = w_mat, 
        p_deriv_mat = p_deriv_mat, w_adf = w_adf, res_u = res_u, 
        N = length(sem.data[, 1]), ginvFlag=ginvFlag)
    class(ret) <- "adjchisq"
    return(ret)
}
