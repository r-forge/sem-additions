model.cov<-function(ram.object, input.data){
	
	newdata<-model.data(ram.object, input.data)
	cov.matrix<-var(newdata, na.rm=T)
	
	return(cov.matrix)
}
