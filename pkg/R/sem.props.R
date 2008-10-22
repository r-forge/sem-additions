###
# sem.props
#
# extracts and created a few useful values
# for use by a variety of other functions
#
# last updated 8/18/08
###

sem.props <- function (object){
	ret<-list()
	ret$N <- object$N
	N<-ret$N
	ret$n <- object$n
	n<-ret$n
	ret$t <- object$t
	t<-ret$t
	ret$n.fix <- object$n.fix
	n.fix<-ret$n.fix
	ret$df <- n*(n + 1)/2 - t - n.fix*(n.fix + 1)/2
	ret$chisq <- object$criterion * (N - (!object$raw))
	return(ret)
	}
