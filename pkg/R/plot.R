plot.sem<-function(sem.object,  add.error=F, sig.col="black", ns.col="darkgrey",  ...){
	
	g.matrix<-expanded.ram.to.adj.matrix(sem.object$ram)
	
	.GlobalEnv$edge.colors<-sig.col.matrix(sem.object$ram, sig.paths(sem.object), sig.col=sig.col, ns.col=ns.col)
	.GlobalEnv$vertex.names<-sem.object$var.names
	
	
	gplot(g.matrix, edge.curve=0.1, usecurve=T, label=vertex.names, 
			jitter=T,gmode="diagraph", interactive=T, edge.col=edge.colors, ...)
	
}

plot.ram<-function(ram.object, ...){
	g.info<-expand.and.get.vars.ram(ram.object)
	g.matrix<-expanded.ram.to.adj.matrix(g.info$ram)
	
	.GlobalEnv$vertex.names<-as.character(g.info$vars)
	
	gplot(g.matrix, edge.curve=0.1, usecurve=T, label=vertex.names,
	jitter=T, gmode="diagraph",interactive=T, edge.col="black", ...)
}
	
	

sig.paths<-function(sem.model, alpha=0.05){
	#get significance of each edge, gakked from summary.sem
	se <- sqrt(diag(sem.model$cov))
   	z <- sem.model$coeff/se
	is.sig <- 2*(1 - pnorm(abs(z))) <  alpha
	
	my.sig.paths<-names(which(is.sig==T))
	return(my.sig.paths)
}

#expands a ram into the matrix that we see in an sem
#object, gakked from sem.r
expand.and.get.vars.ram<-function(ram){
  parse.path <- function(path) {   
        path.1 <- gsub('-', '', gsub(' ','', path))
        direction <- if (regexpr('<>', path.1) > 0) 2 
            else if (regexpr('<', path.1) > 0) -1
            else if (regexpr('>', path.1) > 0) 1
            else stop(paste('ill-formed path:', path))
        path.1 <- strsplit(path.1, '[<>]')[[1]]
        list(first=path.1[1], second=path.1[length(path.1)], direction=direction)
  }
  if ((!is.matrix(ram)) | ncol(ram) != 3) ram[,3]<-NA
  startvalues <- as.numeric(ram[,3])
  par.names <- ram[,2]
  n.paths <- length(par.names)
  heads <- from <- to <- rep(0, n.paths)
  for (p in 1:n.paths){
        path <- parse.path(ram[p,1])
        heads[p] <- abs(path$direction)
        to[p] <- path$second
        from[p] <- path$first
        if (path$direction == -1) {
            to[p] <- path$first
            from[p] <- path$second
            }
  }
  ram <- matrix(0, p, 5)
  vars <- unique(c(to, from))
  pars <- na.omit(unique(par.names))
  ram[,1] <- heads
  ram[,2] <- apply(outer(vars, to, '=='), 2, which)
  ram[,3] <- apply(outer(vars, from, '=='), 2, which)   
  par.nos <- apply(outer(pars, par.names, '=='), 2, which)
  ram[,4] <- unlist(lapply(par.nos, function(x) if (length(x) == 0) 0 else x))
  ram[,5]<- startvalues
  colnames(ram) <- c('heads', 'to', 'from', 'parameter', 'start')
  rownames(ram)<-par.names
 return(list(ram=ram, vars=vars))
}

get.vars<-function(ram.object){
	paths<-ram.object[,1]
	var.list<-list()
	for(path in paths){
		path<-gsub('>','',gsub('<', '', gsub(' ','', path)))
		path<-as.vector(strsplit(path, '-'))[[1]]
		for(element in path){var.list[[element]]<-1}
	}
	
	
	return(names(var.list))
}

expanded.ram.to.adj.matrix<-function(expanded.ram){
	num.vars<-max(c(expanded.ram[,2], expanded.ram[,3]))
	adj.matrix<-matrix(rep(0,num.vars^2), ncol=num.vars)
	for (a.row in 1:length(expanded.ram[,1])){
		adj.matrix[expanded.ram[a.row,3],expanded.ram[a.row,2]]<-1
	}
	
	return(adj.matrix)
}

sig.col.matrix<-function(expanded.ram, are.sig,
					sig.col="black", ns.col="darkgrey"){
					
	num.vars<-max(c(expanded.ram[,2], expanded.ram[,3]))
	adj.matrix<-matrix(rep("darkgrey",num.vars^2), ncol=num.vars)

	for (a.row in 1:length(expanded.ram[,1])){
		if((rownames(expanded.ram)[a.row]) %in% are.sig) {
			adj.matrix[expanded.ram[a.row,3],expanded.ram[a.row,2]]<-"black"
		}
	}
	
	return(adj.matrix)
}
