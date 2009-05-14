rmsea.power <- function(rmsea.ha, df, n, rmsea.h0=0.05, alpha=0.05) {
	ncp0 <- (n-1) * df * rmsea.h0 * rmsea.h0
	ncpa <- (n-1) * df * rmsea.ha * rmsea.ha
	if (rmsea.h0 < rmsea.ha) {
		cval <- qchisq(1-alpha, df, ncp0)
		return(1-pchisq(cval, df, ncpa))
	}
	if (rmsea.h0 >= rmsea.ha) {
		cval <- qchisq(alpha, df, ncp0)
		return(pchisq(cval, df, ncpa))
	}
}

