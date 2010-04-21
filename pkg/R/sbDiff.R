#last modified 4/20/2010

sbDiff<-function(sbobj.A, sbobj.B) anova.adjchisq(sbobj.A, sbobj.B)

#sbDiff<-function(sbobj.A, sbobj.B){
#	
#	#guard against specifying the wrong order of who is nested
#	if(sbobj.A$df < sbobj.B$df){m0<-sbobj.B; m1<-sbobj.A #B is nested
#		} else	{m0<-sbobj.B; m1<-sbobj.A} #A is nested
#
#	df<-m0$df - m1$df
#	
#	#calculation of cd from http://www.statmodel.com/chidiff.shtml
#	cd<-(m0$df * m0$c - m1$df * m1$c)/(m0$df - m1$df)
#	
#	TRd <- (m0$chisq - m1$chisq)/cd
#	
#	p<-1-pchisq(TRd, df)
#	
#	retmat<-matrix(c(TRd, df, p), nrow=1)
#	rownames(retmat)<-""
#	colnames(retmat)<-c("Chi Sq", "DF", "p")
#	
#	retmat
#	}