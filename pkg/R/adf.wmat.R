
#calculate ADF weight matrix from raw data
#this is actually waaaay faster, and produces correct output.  The Bentler adapted code was giving me the wrong matrix for some reason...
#last updated 4/20/10

adf.wmat<-function(raw_data){
	n<-length(raw_data[,1])	
	n.col<-length(names(raw_data))
	nc.star<-n.col*(n.col+1)/2
	nc2<-n.col^2
	
	i1<-rep(1, n)
	xbar<-apply(raw_data, 2, sum)/n
	z<-raw_data-kronecker(xbar, i1)
	
	sc<-list()
	ind<-combn(n.col+1, 2) #all possible 2 index combinations
	ind[2,]<-ind[2,]-1 #to allow for variances
	
	for(q in 1:nc.star){
		i<-ind[1,q]
		j<-ind[2,q]	
		a.name<-paste(names(raw_data)[i],names(raw_data)[j],sep="_")
				
		sc[[a.name]]<-z[,i]*z[,j]
	}
	adf_mat<-var(data.frame(sc))* (n-1)/n #to correct for n-1
	return(adf_mat)
}


#get sums of squares and cross products
sscp<-function(raw_data){
	n<-length(raw_data[,1])		
	cov.mat<-cov(raw_data)
	sscp.mat<-cov.mat*n
	return(sscp.mat)
}


