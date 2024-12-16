source("Functions/helper_functions.R")
data <- readRDS("Data/data_california.rds")
acres <- data$Acres
max_acres <- max(acres)
p <- 0.9
empsurv <- emp_surv(acres) 
#Plot the empirical survival function
plot(acres, empsurv, 
     xlab = "Acres", 
     ylab = expression(1 - F(x)), 
     main = expression("Survival Function of acres burned in California wildfires from 2003 to 2024"))
abline(v = max_acres, col = "red", lwd = 2)

q_acres <- quantile(acres, p = p)
exceedances_loss <- acres[ which(acres> q_acres)  ] - q_acres
lexceedances <- length(exceedances_loss)

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
init_params <- c(10,1)
interm <- optim(init_params, exc_loss = exceedances_loss,  Likelihood )$par
params <- list(gamma = interm[1] , alpha = interm[2])



##QQ-plot
gamma_hat <- params[[1]]
alpha_hat <- params[[2]]
gen_par_quantile  <- alpha_hat * ( (1-(1:lexceedances/(lexceedances+1)))^(-gamma_hat)   -1    )   / gamma_hat
emp_quantile <- sort(exceedances_loss) 

plot(gen_par_quantile, emp_quantile, 
     main = "Quantile-Quantile Plot", 
     xlab = "Data Quantiles", 
     ylab = "Empirical Quantiles")

# Add the y = x line
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)


#How unusual is the August complex wildfire?

est_prob <- 0.9 * (1 + (gamma_hat *  ( max_acres - q_acres  )   / alpha_hat ) )^(-1/gamma_hat)

