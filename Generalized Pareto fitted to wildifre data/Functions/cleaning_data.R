install.packages("readxl")  # Install package
library(readxl)             # Load package
data <- read_excel("C:/Users/mourahib/Desktop/Postdoc/data_wildfire_nevada.xlsx")
colnames(data)[colnames(data) == "Ignition Date"] <- "Date"
data$Year <- format(data$Date, "%Y")



data <- readRDS(file="Generalized Pareto fitted to wildifre data/Data/shasta_temp.rds")
data$DATE <- as.Date(data$DATE)
data$YEAR <- format(data$DATE, "%Y")
saveRDS(data , file="Generalized Pareto fitted to wildifre data/Data/shasta_temp.rds")
sum_temp_byyear <- aggregate(data$TOBS~data$YEAR , data , mean)

plot(sum_temp_byyear)
