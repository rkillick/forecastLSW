sig.sq <-function(predeq, minimiser = solve(predeq$B, predeq$RHS)){
  
  # input checks
  if(!is(predeq,'predeq')){stop('predeq must be of class "predeq" as output from predeq.est.')}
  if(!is.numeric(minimiser)){stop('minimiser must be numeric.')}
  
  # rest of function
  s <- length(predeq$RHS)
  B <- matrix(0, s + 1, s + 1)
  B[1:s, 1:s] <- predeq$B
  B[1:s, s + 1] <- predeq$RHS
  B[s + 1, 1:s] <- predeq$RHS
  B[s + 1, s + 1] <- predeq$extra.var
  b <- c(minimiser, -1)
  sqerr <- sum(b * (B %*% b))
  return(sqerr)
}
