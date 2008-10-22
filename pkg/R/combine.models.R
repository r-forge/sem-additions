
###
# Smooshes multiple RAM models together into one
#
# Last updated 8/18/08
###

combine.models<-function(...){
	models<-list(...)
	paths<-vector()
	pathnames<-vector()
	starts<-vector()

	for (i in 1:length(models)){
		paths<-c(paths, models[[i]][,1])
		pathnames<-c(pathnames, models[[i]][,2])
		starts<-c(starts, models[[i]][,3])
		}
	new.mod<-matrix(t(c(paths, pathnames, starts)), ncol=3)
	class(new.mod)<-"mod"
	return(new.mod)
}
