source("Functions/Likelihood_gpd.R")
source("Functions/helper_functions.R")
data <- readRDS("Data/data_california.rds")
acres <- data$Acres
max_acres <- max(acres)
p <- 0.9
empsurv <- emp_surv(acres) 


# Plot the survival function
plot(acres, empsurv, 
     xlab = "Acres", 
     ylab = expression(1 - F(x)), 
     main = expression("Survival function of acres burned in California wildfires from 2003 to 2024"))

# Change the maximum point to red
points(max(acres), empsurv[which.max(acres)], col = "red", pch = 16, cex = 1.2)

legend("topright", legend = "August Complex Wildfire", col = "red", pch = 16, cex = 0.9)


q_acres <- quantile(acres, p = p)
exceedances_loss <- acres[ which(acres> q_acres)  ] - q_acres
lexceedances <- length(exceedances_loss)




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
     xlab = "gpd quantiles", 
     ylab = "Empirical quantiles")

# Add the y = x line
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)


#How unusual is the August complex wildfire?

est_prob <- 0.9 * (1 + (gamma_hat *  ( max_acres - q_acres  )   / alpha_hat ) )^(-1/gamma_hat)
print(est_prob)


