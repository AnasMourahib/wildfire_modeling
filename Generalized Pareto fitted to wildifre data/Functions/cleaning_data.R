install.packages("readxl")  # Install package
library(readxl)             # Load package
data <- read_excel("C:/Users/mourahib/Desktop/Postdoc/data_wildfire_nevada.xlsx")
colnames(data)[colnames(data) == "Ignition Date"] <- "Date"
data$Year <- format(data$Date, "%Y")
