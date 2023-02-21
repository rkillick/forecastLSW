reg.xyr=function(pr){
  # Function to implement regularization from Xie, Yu, Ranneby 2007
  # pr is output from pred.est.gw
  
  # no input checks as this function is not exported and check are done in parent function.
  
  fn=function(lambda){
    is.positive.definite=function(lambda){
      val = try(chol(pr$B-diag(lambda,nrow=nrow(pr$B),ncol=ncol(pr$B))), silent = TRUE)
#      if (class(val) == "try-error") {
	# MAN edited:
      if ("try-error" %in% class(val)) {
        return(FALSE)
      }
      else {
        return(TRUE)
      }      
    }
    check=is.positive.definite(lambda) # checks satisfies (18) from xyr paper
    if(check){
      inv.bracket=solve(pr$B-diag(lambda,nrow=nrow(pr$B),ncol=ncol(pr$B)))
      return((pr$RHS%*%inv.bracket%*%inv.bracket%*%pr$RHS -1)^2)
    }
    else{return(Inf)}
  }
  
  if(is.matrix(pr$B)){
    op.lambda=suppressWarnings(optimize(fn,interval=c(-1,1))$minimum)
    pr$B=pr$B-diag(op.lambda,nrow=nrow(pr$B),ncol=ncol(pr$B))    
  }# if not a matrix then there is no problems with regularity and thus nothing to be done.
  return(pr)
}
