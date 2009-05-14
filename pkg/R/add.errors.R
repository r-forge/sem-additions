####
# function to fill in errors for variables that still need it
# rather than get an error from sem
#
# Last Updated: 8/18/08
###



#takes a RAM model, and spits back all of the variables that don't already have an error term
get.vars.that.need.error<-function(model, add.exo=FALSE){
	
    ##First, find out what variables need an error term
	variables<-list()
	
	#go through all of the paths and pull out variables
	for (paths in model[,1]){
		vars<-strip.white(paths)#take away any pesky spacing
		vars<-sub("<->", "->", vars) #so that all arrows are the same
		vars<-strsplit(vars, "->")#now split it into two variables

		#first, check to see if this is for something that already has an error term
		if(vars[[1]][1]  != vars[[1]][2]){

		  #if not, check to see that both variables in the path exist in the variables table
		  for(a.variable in vars[[1]]){

			#if the variable does not exist in the table, set them to 1
			if(length(variables[[a.variable]]>0) == 0) variables[[a.variable]]<-1
		  }
		  
		}else{
		  #make sure we know that this variable already has an error
		  variables[[ vars[[1]][1] ]]<-2 #placeholder for "I already have an error!"
		}   
	
	}
	
	#exogenous variables don't get error terms!
	if(!add.exo){
	 exo.vars<-find.exogenous(model)
	 if(length(exo.vars)>0){
	 	for (i in 1:length(exo.vars)) {variables[[ exo.vars[i] ]] <- 0}
	 }
	}
	
	#return the variables
	variables
}



##
#fills in errors, so you don't have to type all of them
##
add.errors<-function(model, add.exo=FALSE){

   #figure out what variables need error paths
   variables<-get.vars.that.need.error(model, add.exo=add.exo)
	
	#now creat the error paths
	#create blank lists of the appropriate length
	paths<-vector()
	var.names<-vector()
	
	#now iterate over all of the variables, and create the appropriate paths and variable names
	for (element in names(variables)){
		
		#make sure an error term is needed
		if (variables[[element]]==1){
			paths<-c(paths, paste(element, "<->", element, sep=""))
			var.names<-c(var.names, paste(element,".error", sep=""))
		}
	}

	##Now bind it all together into a new RAM model and send it back to the user
	ram<-cbind( c(model[,1], paths),
				c(model[,2], var.names),
				c(model[,3], rep(NA, length(paths)))
			  )
	class(ram)<-"mod"
	ram
}
