#last modified 4/10/10
#changelog
# 4/10/10 - added na.rm argument
model.cov<-function(ram.object, input.data, na.rm=T, use="complete.obs"){
	
	newdata<-model.data(ram.object, input.data)
	cov.matrix<-var(newdata, na.rm=na.rm, use=use)
	
	return(cov.matrix)
}
