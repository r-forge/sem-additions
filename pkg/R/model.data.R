###
# creates a data from from a subset of data
# that is used in a ram model
#
# Last Updated 8/18/08
##
model.data<-function(ram.object, input.data){
	var.list<-get.vars(ram.object)
	newdata<-list()
	for(variable in var.list){
		newdata[[variable]]<-input.data[[variable]]
	}
	
	newdata<-data.frame(newdata)
	return(newdata)
}
