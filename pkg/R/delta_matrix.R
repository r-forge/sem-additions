##
#Delta macro - Computes the partial derivatives of the model parameters with
#  respect to the non-duplicated fitted covariances
# Adapted from SPSS code by Peter Bentler
#                 
# Changelog
#5/2/10 - updated by John Fox for efficiency
# 4/28/10 - modified to look at paramters instead of individual paths
# 3/5/10 - fixed bug of not adjusting for correlated disturbance on both parts of P matrix 
# 2/8/10 - fixed bug of using fixed params
# 9/1/08 - fixed matrix selection bug
##

delta_matrix <- function (sem.object, adj = 1e-04) {
	p.star <- sem.object$n * (sem.object$n + 1)/2
	pars <- names(sem.object$coeff)
	nparms <- length(pars)
	delta.mat <- matrix(0, nparms, p.star)
	rownames(delta.mat) <- pars
	vars <- sem.object$var.names
	J <- sem.object$J
	m <- sem.object$m
	for (j in 1:nparms) {
		A <- sem.object$A
		P <- sem.object$P
		i <- which(rownames(sem.object$ram) == pars[j])
		from <- sem.object$ram[i, 2]
		to <- sem.object$ram[i, 3]
		path_type <- sem.object$ram[i, 1][1]
		if (path_type == 1){
			AA <- A[cbind(from, to)][1]
			adjust <- abs(AA) * adj
			A[cbind(from, to)] <- AA + adjust
		}
		else {
			PP <- P[cbind(to, from)][1]
			adjust <- PP * adj
			P[cbind(from, to)] <- P[cbind(to, from)] <- PP +
adjust
		}
		I.Ainv <- solve(diag(m) - A)
		C <- J %*% I.Ainv %*% P %*% t(I.Ainv) %*% t(J)
		delta.mat[j, ] <- (vech(C) - vech(sem.object$C))/adjust
	}
	t(delta.mat)
}
