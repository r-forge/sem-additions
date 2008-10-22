###
# looks through a model, and generates a list of all 
# of the exogenous variables based on those that do not currently 
# have any paths pointing to them
###

find.exogenous<-function(model){
  variables<-list()

  #read in a line of the model
  for (paths in model[,1]){
  	
	vars<-strip.white(paths)#take away any pesky spacing

	#determine if this is a path
	is.path<-FALSE
	if(length(grep("<->", vars))) {is.path<-TRUE}
	
	#record the name of both variables in the variable hash
	vars<-sub("<->", "->", vars) #so that all arrows are the same
	vars<-strsplit(vars, "->")#now split it into two variables

	#create an entry for the both variables, if we need to
	if(is.null(variables[[ vars[[1]][1] ]])) variables[[ vars[[1]][1] ]] <- FALSE
	if(is.null(variables[[ vars[[1]][2] ]])) variables[[ vars[[1]][2] ]] <- FALSE	

	#check to make sure this isn't an error term or a covariance
	if(vars[[1]][1]!=vars[[1]][2] && !(is.path)){

	#set the second variable is set to true, as it is endogenous
	variables[[ vars[[1]][2] ]] <- TRUE


	}
   }
	
  #return the list of exogenous variables
  exo.vars<-names(subset(variables, variables==FALSE))
  return(exo.vars)
}

