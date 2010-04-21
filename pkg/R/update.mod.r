#####
# update.mod
# updates a ram model
# by addin, deleting, or replacing elements
#
# uses scan to get a list of modifications
#
# to delete, the format for a line is
# delete, NAME, SORT
# where name is the name of a variable, path, or coefficient
# and sort is one of those three words
#
# to add a new path, simply use the following format
# add, a->b, coef.name, start.value
# where, aside from the word add, the rest is the same as a line
# in specify.model()
#
# to replace one variable with a different one, use the following
# replace, OLD, NEW
# where old is the name of a variable currently in the model
# and new is the name of the variable to which you would like to change it
#
# Last updated 3/1/2010
#####

update.mod<-function(object, file=""){
  modmat<-scan(file = file, what = list(change="", var1 = "", var2 = "", 
        var3 = "", var4=""), sep = ",", strip.white = TRUE, 
        comment.char = "#", fill = TRUE)
  modmat<-cbind(modmat$change, modmat$var1, modmat$var2, modmat$var3)
 
  #####ADDITIONS
  
  if ("add" %in% modmat[,1]){
  	addmat<-modmat[which(modmat[,1]=="add"), 2:4]

	#length check in case only one thing is being added
  	if(length(addmat)==3) addmat <-matrix(addmat, ncol=3)
  	class(addmat)<-"mod"
  	
  	#combine the models
  	object<-combine.models(object, addmat)
  	}
  	
  #######DELETIONS
  
	######
	# delete.model.element
	#
	# a local function
	# takes care of deleting elements from a ram object
	######

	delete.model.element<- function (delete.text, old.model, type = "path") {
		#a failed match provides an infomative enough error message
		type<-match.arg(type, c("path", "variable", "coefficient"))
		col.index<-list("path"=1, "variable"=1, "coefficient"=2)[[type]]
		delete.text <- strip.white(delete.text)
	     old.model<-old.model[which(is.na(pmatch(strip.white(old.model[,col.index]), delete.text))),]
	     class(old.model)<-"mod"
	     return(old.model)
	}
  	  	
  if("delete" %in% modmat[,1]){

  	deletemat<-modmat[which(modmat[,1]=="delete"), 2:3]
  	#length check in case only one thing is being deleted
  	if(length(deletemat)==2) deletemat <-matrix(deletemat, ncol=2)

	#scroll through and delete all relevant entities
  	for (i in 1:length(deletemat[,1])) 
  		object<-delete.model.element(deletemat[i,1], object, deletemat[i,2])
  	}
  	#delete.model.element("theta1", model.dhp, type="variable")
  
  #############REPLACE
  #to substitude one variable in for another
  
  if("replace" %in% modmat[,1]){
  	submat<-modmat[which(modmat[,1]=="replace"),2:3]
  	#the variable is just to keep things quiet
	mapply( function(x,y) object[,1:2]<<-gsub(x, y, object[,1:2]), submat[,1], submat[,2])  
	}	
  return(object)
}



#a quick demo
#model.dhp <- specify.model()
#    RParAsp  -> RGenAsp, gam11,  NA
#    RIQ      -> RGenAsp, gam12,  NA
#    RSES     -> RGenAsp, gam13,  NA
#    FSES     -> RGenAsp, gam14,  NA
#    RSES     -> FGenAsp, gam23,  NA
#    FSES     -> FGenAsp, gam24,  NA
#    FIQ      -> FGenAsp, gam25,  NA
#    FParAsp  -> FGenAsp, gam26,  NA
#    FGenAsp  -> RGenAsp, beta12, NA
#    RGenAsp  -> FGenAsp, beta21, NA
#    RGenAsp  -> ROccAsp,  NA,     1
#    RGenAsp  -> REdAsp,  lam21,  NA
#    FGenAsp  -> FOccAsp,  NA,     1
#    FGenAsp  -> FEdAsp,  lam42,  NA
#    RGenAsp <-> RGenAsp, ps11,   NA
#    FGenAsp <-> FGenAsp, ps22,   NA
#    RGenAsp <-> FGenAsp, ps12,   NA
#    ROccAsp <-> ROccAsp, theta1, NA
#    REdAsp  <-> REdAsp,  theta2, NA
#    FOccAsp <-> FOccAsp, theta3, NA
#    FEdAsp  <-> FEdAsp,  theta4, NA
#
#update(model.dhp)
#add, newvar -> FEdASP, gam71, NA
#add, newvar <-> newvar, theta5, NA
#delete, FSES, variable
#delete, theta1, variable #this should fail
#delete, theta2, coefficient #this should not
#delete, RIQ -> RGenAsp, path
#replace, FEdAsp, HELLO!
#replace, ROccAsp, GOODBYE

