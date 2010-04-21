###
#from http://www.statmodel.com/chidiff.shtml
# Satorra-bentler adjusted chi sq
# last updated 4/20/10
###
anova.adjchisq<-function(adjobj0, adjobj1){
	
	#create an output table
	out.mat<-matrix(ncol=5, nrow=2)
	rownames(out.mat)<-c("Model 1", "Model 2")
	colnames(out.mat)<-c("Model DF", "Model Chisq", "Df", "LR Chisq", "Pr(>Chisq)")
	
	out.mat[1,1]<-adjobj0$df
	out.mat[1,2]<-adjobj0$chisq.scaled
	
	out.mat[2,1]<-adjobj1$df
	out.mat[2,2]<-adjobj1$chisq.scaled
	
	#switching to get order right
	sbs.nested<-adjobj0
	sbs.full<-adjobj1
	if(adjobj0$df<adjobj1$df){
		sbs.nested<-adjobj1
		sbs.full<-adjobj0
		}
		
	
	
	t0<-sbs.nested$chisq
	tr0<-sbs.nested$chisq.scaled
	t1<-sbs.full$chisq
	tr1<-sbs.full$chisq.scaled
	
	c0<-sbs.nested$c
	c1<-sbs.full$c
	
	d0<-sbs.nested$df
	d1<-sbs.full$df
	
	cd = (d0 * c0 - d1*c1)/(d0 - d1)
	trd = (t0 - t1)/cd 
	
	out.mat[2,3]<-d0-d1
	out.mat[2,4]<-trd
	out.mat[2,5]<-1-pchisq(trd, d0-d1)
	
	print(out.mat, na.print = "")
	
	}