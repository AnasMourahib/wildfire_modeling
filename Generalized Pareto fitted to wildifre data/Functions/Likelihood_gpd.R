Likelihood <- function(params, exc_loss) {
  gamma <- params[1]
  alpha <- params[2]
  lexc <- length(exc_loss)
  vec <- 1+ (gamma*  exc_loss/alpha)    
  if( alpha < 0 || any(vec< 0)   )  
  {
    return(10^(16))
  }
  if(gamma != 0){
    S <- log(1+ (gamma*  exc_loss/alpha) ) 
    Likelihood <-  -1 *lexc * log(alpha) -(1+ (1/gamma)) * sum(S)
  } 
  return(-Likelihood)
}

