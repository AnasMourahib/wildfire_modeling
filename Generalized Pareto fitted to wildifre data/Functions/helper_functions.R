emp_surv <- function(vec){
  l <- length(vec)
  cdf <- rep(0,l)
  for(i in 1: l){
    cdf[i] <-1- (1/l)*(length(which(vec <= vec[i])))
  }
  return(cdf)
} 
