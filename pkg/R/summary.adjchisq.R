###
# prints a Chisq Table	
#
# Laste Updated 8/18/08
###

summary.adjchisq<-function(adj.obj){
	chisq.mat<-with(adj.obj, matrix(c(chisq, df, p.old, c, chisq.scaled, p), nrow=1))
	colnames(chisq.mat)<-c("Chi Sq", "DF", "p", "C", "SB Corrected Chisq", "p")
	rownames(chisq.mat)<-c("")
	print(chisq.mat)
	cat("\n")
	if(!(is.null(adj.obj$d_c))) cat(paste("Corrected df", adj.obj$d, sep=" = "))
	}
	
print.adjchisq<-function(adj.obj) summary.adjchisq(adj.obj)
