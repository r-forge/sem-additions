
#takes a model and splits it into lines
split.model<-function(a.model){
	modvec<-strsplit(a.model, "\n")[[1]]
	return(modvec)
}

#clean up a model line and return a formula only
clean.modline<-function(modline){
	modline<-gsub("=", "", modline)
	modline<-gsub("~~", "~", modline)
	return(as.formula(modline))
}

#get the response variable of a formula
response<-function(a.formula){
	gsub("()","", a.formula[2])
	}

#get the predictor variables of a formula
predictors<-function(a.formula){
#	terms(a.formula)[[3]]
	gsub("()","", a.formula[3])
	}
	
#what kind of operator is a formula using?
getOperator<-function(a.formula){
	op<-NA
	for (a.op in c("~", "~~", "=~"))
		if(grepl(a.op, a.formula, perl=T)) op<-a.op
	return(op)
	}


#####
#update a lavaan model
#####	
update.model<-function(old.model, changes){
	#first, break changes into list of response variables, operators, and updates
	#then put it into a list for comparison against the old model
	changes<-split.model(changes)
	changelist<-list()
	for (i in 1:length(changes)){
	  op<-getOperator(changes[i])
	  if(!is.na(op)){
	  	a.formula<-clean.modline(changes[i])
	  	changelist[[paste(response(a.formula), op, sep="")]]<-predictors(a.formula)
	   }
	  }
	
	#now, run through the old model, and change things as needed
	old.model<-split.model(old.model)
	newmodel<-""
	for(i in 1:length(old.model)){
	  #first, see if anything in the list matches
	  newline<-old.model[i]
	  op<-getOperator(old.model[i])
	  if(!is.na(op)){
	  	a.formula<-clean.modline(old.model[i])
	  	lookup<-changelist[[paste(response(a.formula), op, sep="")]]
	  	if(!is.null(lookup)){
	  	  #OK, now, update the formula
	  	  a.formula<-update(a.formula, paste(response(a.formula),"~", lookup, sep=""))
	  	  
	  	  #now make a new line
	  	  #strips off comments and other whitespacing...fix later
	  	  newline<-paste(response(a.formula), op, predictors(a.formula))

	  	  #making sure we have't stripped out everything
	  	  #this will mess with means only
	  	  #fix this later
	  	  if(predictors(a.formula)==1) newline=""

	  	  }

	  }
	 newmodel<-paste(newmodel, newline, "\n", sep="") 
	}
	
  return(newmodel)
}
#
###example
#a.model<-'y~x+z
#		  x~z
#		  z=~a+b+c
#		  y~~c
#'
#
#some.changes<-'y~.-x
#		y~~.-c
#		x~.+y
#'
#
#update.model(a.model, some.changes)

#####
#turn a lavaan model
#into a ram object for the sem package
#####
model.to.ram<-function(a.model){
  require(sem.additions)
  a.model<-split.model(a.model)
  ram<-matrix(ncol=3)

  #for each line in the model
  for(i in 1:length(a.model)){
   op<-getOperator(a.model[i])
   
   #make sure this is really a model line
   #and now a comment or whitespace
   if(!is.na(op)){
   	
    #what kind of path is this?	
    pathType<-"->"
    if(op=="~~") pathType <-"<->"
    a.formula<-clean.modline(a.model[i])

	#is this a latent or observed variable?
    if(op=="=~"){
    		x<-response(a.formula)
    		y<-strip.white(strsplit(predictors(a.formula), "+", fixed=T)[[1]])
    		ram<-rbind(ram, c( paste(x,"<->",x, sep=""),  paste(x,"error", sep="."), NA))
    	}else{
    		y<-response(a.formula)
    		x<-strip.white(strsplit(predictors(a.formula), "+", fixed=T)[[1]])
    		}
  
    #iterate over each response, in case this is a latent variable
    for (q in 1:length(y)){
     #iterate over each predictor variable
     for(j in 1:length(x)){
     	#see if this is a fixed parameter values
    	     if(op=="=~"){
 		   isFixed<-strsplit(y[q], '*', fixed=T)[[1]]
		   if(length(isFixed)>1) y[q]<-isFixed[2]
		 }else{
			isFixed<-strsplit(x[j], '*', fixed=T)[[1]]
			if(length(isFixed)>1) x[j]<-isFixed[2]
			}
     	
     	
     	#construct a new path
     	path<-paste(x[j], y[q], sep=pathType)



    	 if(length(isFixed)>1) {
    	   coef<-NA
    	   startValue<-isFixed[1]
    	  #otherwise....
    	  }else{
    	   #create a new coefficient name
    	   coef<-paste(x[j], y[q], sep=".")
    	   if(pathType=="<->") {
    	   	ifelse((x[j]==y[q]),
    	   		coef<-paste(coef, "var", sep="."), 
    	   		coef<-paste(coef, "cov", sep="."))
    	   	}
    	   startValue<-NA
    	  }
    	 ram<-rbind(ram, c(path, coef, startValue))
    	 }
    	}
   }
  }
  #clean off the NA row
  ram<-ram[2:length(ram[,1]),]
  class(ram)<-"mod"
  #add disturbance terms where needed
  #using sem.additions code
  ram<-add.errors(ram)
  return(ram)	
}



###example
a.model<-'y~x+z
		  x~3*z
		  z=~a+b+c
		  y~~c
'

cat(a.model)
model.to.ram(a.model)


###
#can we go back?
#we'll need to know which are latent variables
#as there is no way of telling from the ram formulation
###
ram.to.model<-function(ram.obj, latents=NA){
 # a function to determine if a value is fixed
 #and return the proper predictor for a model
 getFix<-function(a.row){
 	if(is.na(a.row[2])) return(paste(a.row[3], "*", sep=""))
 	return("")
 	}

 model_list<-list()
 for(i in 1:length(ram.obj[,1])){
 	ram.obj[i,1]<-strip.white(ram.obj[i,1])
 	iscov<-grepl("<->", ram.obj[i,1], perl=T)
 	vars<-strsplit(ram.obj[i,1], c("->", "<->")[as.numeric(iscov)+1])[[1]]

	op<-"~"
	#is this a latent variable
	if(vars[2] %in% latents) op<-"=~"
	
	#is this a covariance term
	if(iscov) op<-"~~"
	
	newKey<-paste(vars[2], op, sep="")

	#is it in the model list?
	if(is.null(model_list[[newKey]])){
	#if not, start a new entry in the model lists
	model_list[[newKey]]<-paste(getFix(ram.obj[i,]), vars[1], sep="")
	}else{	
	#add the variable otherwise
	model_list[[newKey]]<-paste(model_list[[newKey]], "+",getFix(ram.obj[i,]), vars[1], sep="")
	} 	
 }
 a.mod<-""
 for (j in 1:length(names(model_list))){
 	a.mod<-paste(a.mod, names(model_list)[j], model_list[[j]], "\n", sep="")
 }
 return(a.mod)	
}

###example
#a.ram<-specify.model()
#a->b, ab, NA
#b->c, bc, NA
#d->c, NA, 3
#a<->c, ac.cov, NA
#
#cat(ram.to.model(a.ram, latents="b"))