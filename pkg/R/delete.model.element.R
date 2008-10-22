##
#deletes paths, variables, or path with certain coefficients out of a model
#
# Last updated 8/18/08
##

delete.model.element<-function (delete.text, old.model, sort.by="path") {

	#determine what sort of model element we'll be evaluating
	col.index<-0
	if(sort.by=="path"){col.index<-1}
	if(sort.by=="variable"){col.index<-1}
	if(sort.by=="coefficient"){col.index<-2}
	if(col.index==0) stop("Cannot delete a ", sort.by)

	#blank vectors to start with
	col1<-vector()
	col2<-vector()
	col3<-vector()

	#purge spaces from delete.text
	delete.text<-strip.white(delete.text)
	
	for (line.num in 1:length(old.model[,1])){
		#purge spaces from the relevant line, just in case this is a path
		old.model[line.num, col.index]<-strip.white(old.model[line.num, col.index])
		
		#see if this is a line to be deleted
		if(!(delete.text %p.in% old.model[line.num, col.index])){
			col1<-c(col1,old.model[line.num,1])
			col2<-c(col2,old.model[line.num,2])
			col3<-c(col3,old.model[line.num,3])
		}
	}
	
	#now turn the columns into a RAM model
	ram<-cbind(col1, col2, col3)
	class(ram)<-"mod"
	return(ram)

}