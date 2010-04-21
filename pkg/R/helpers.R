
#####################################
# a few helper					#
# functions for 					#
# later use in the code				#
# Last updated 8/18/08
#####################################

#does partial matching
"%p.in%" <- function(x, y) length(grep(x, y)) > 0
#"b" %p.in% "a->b"
#strips spaces out of strings
strip.white<-function(x) gsub(' ', '', x)


#Matrix Trace
tr<-function(a.mat) sum(diag(a.mat))

#identity matrix generation
ident.mat<-function(n){
	a.mat<-matrix(rep(0, n^2), nrow=n)
	for(x in 1:n) a.mat[x,x]<-1
	return(a.mat)
	
}

#vech, as the ks vech borks on certain valid matrices due to type issues
# code based on ks library
#now using matrixcalc code
#vech<-function (x) 
#{
#    d <- ncol(x)
#    vechx <- vector()
#    for (j in 1:d) vechx <- c(vechx, x[j:d, j])
#    return(vechx)
#}

#invvech, so as to remove dependency on ks - code based on ks library
invvech<-function (x) 
{
    d <- (-1 + sqrt(8 * length(x) + 1))/2
    invvechx <- matrix(0, nr = d, nc = d)
    for (j in 1:d) invvechx[j:d, j] <- x[1:(d - j + 1) + (j - 
        1) * (d - 1/2 * (j - 2))]
    invvechx <- invvechx + t(invvechx) - diag(diag(invvechx))
    return(invvechx)
}

#now using code from matrixcalc
#vec<-function (x, byrow = FALSE) 
#{
#    if (byrow) 
#        x <- t(x)
#    d <- ncol(x)
#    vecx <- vector()
#    for (j in 1:d) vecx <- c(vecx, x[, j])
#    return(vecx)
#}


#gets names of each row_col of a matrix
matrix.names<-function(mat, sep="_"){
	rnames<-rownames(mat)
	cnames<-colnames(mat)
	for(i in 1:length(mat[,1])){
		for(j in 1:length(mat[,1])){
			mat[i,j]<-paste(rnames[i], cnames[j], sep=sep)
		}
	}
	return(mat)
}

#centers a data frame around the mean of each value
centmean<-function(raw_data){
	n<-length(raw_data[,1])	
	i1<-rep(1, n)
	xbar<-apply(raw_data, 2, sum)/n
	z<-raw_data-kronecker(xbar, i1)
	return(z)
}
