source("Functions/Likelihood_gpd.R")
source("Functions/helper_functions.R")
data <- readRDS("Data/data_california.rds")
acres <- data$Acres
max_acres <- max(acres)
p <- 0.95
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


######Plot the acres burned each year from 1984 until 2023 in california

install.packages("lubridate")
library(lubridate)
data$Date <- as_datetime(data$Date)  # Convert to datetime format

# Filter the data to include only wildfires
data_wildfire_only <- data[data$`Fire Type` == "Wildfire", ]

# Aggregate the data by year and sum the area_burned
wildfires_by_year <- aggregate(Acres~ year, data = wildfire_only, sum)

# Load ggplot2 package
library(ggplot2)

# Create a bar plot using ggplot2
ggplot(wildfires_by_year, aes(x = year, y = Acres)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Total Acres Burned by Wildfires Each Year in California 1984-2023",
       x = "Year", y = "Acres Burned") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


