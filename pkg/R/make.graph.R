
#takes an sem object, and creates a graph from it
make.graph.sem<-function(sem.object, add.error=F){
	#the below works on expanded RAM matrices
	#that sem generates  - this is so that we can
	#graph both sem objects and ram objects
	g<-make.graph.expanded.ram(sem.object$ram, sem.object$var.names, add.error=add.error)
	return(g)
}


make.graph.ram<-function(ram.object, add.error=F){
	expansion<-expand.and.get.vars.ram(ram.object)
	g<-make.graph.expanded.ram(expansion$ram, expansion$vars, add.error=add.error)
	return(g)
}



make.graph.expanded.ram<-function(ram, var.names, add.error=F){

	#create the vertices in the graph
	g<-graph.empty(directed=T)
	g<-add.vertices(g, length(var.names), names=as.character(var.names))
	
	#create a list so that we can match variables to paths
	ids<-list()
	#note, we're dealing in base 0
	ids <- 1:length(V(g)$names)-1
	names(ids)<-var.names
	
	edge.matrix<-matrix(c(ram[,3]-1, ram[,2]-1), ncol=2)
	if(!(add.error)){edge.matrix<-edge.matrix[(which(edge.matrix[,1]!=edge.matrix[,2], arr.ind=F)),]}
	g<-add.edges(g, t(edge.matrix))
	
	return(g)
}

