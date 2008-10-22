##
#adds new lines to an existing RAM model
#
# Last updated 8/18/08
##

add.to.model<-function (old.model, file="", ...){
    #create a ram of the new model
    new.model.lines<-specify.model(file, ...)

	#combine the models
	ram<-combine.models(old.model, new.model.lines)
	
	#now make them into a RAM object
	class(ram) <- "mod"
	ram
}