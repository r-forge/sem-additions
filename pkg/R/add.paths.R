add.paths<-function(from, to, values=rep(NA, length(from)*length(to))){
	newram<-matrix(rep(NA, 3*length(from)*length(to)), ncol=3)
	par.index<-0
	for (i in 1:length(to)){
	  for (j in 1:length(from)){
		par.index<-par.index+1
		newram[par.index,1]<-paste(from[j], "->", to[i], sep="")
		newram[par.index,2]<-paste(from[j], ".", to[i], sep="")
		newram[par.index,3]<-values[par.index]
		}
	}
	
	class(newram)<-"mod"
	return(newram)
}
