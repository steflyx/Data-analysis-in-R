##########################################################################################
# ASSIGNMENT 2 - 6
# 
# DATASET: 
#################################################################################

#Packages
library(FactoMineR)
library(factoextra)

#Introductory work
weather <- read.csv("./Dataset_weather/weatherAUS.csv")
weather <- na.omit(weather)

#Eliminate useless column
weather <- weather[,1:21]

#Make all variables quantitative
weather$WindGustDir <- weather$WindGustDir.numeric
weather$WindDir9am <- weather$WindDir9am.numeric
weather$WindDir3pm <- weather$WindDir3pm.numeric
weather$RinToday <- weather$RainToday.numeric

#Take a sample to limit the amount of computation
weather <- weather[1:2000,]

#########################################################################################

#Question 1)

#Compute PCA
weather_pca <- PCA(weather[,3:18], ncp = 3, graph = FALSE)

#Compute HCPC on PCs
weather_hcpc <- HCPC(weather_pca, graph = TRUE)
weather_hcpc$desc.var

###########################################################################################

#QUestion 2)

#Let's perform the clustering using 3 clusters
k_results <- kmeans(weather[,3:18], center = 3)$centers

#Let's plot them
barplot(k_results[,2], main = "MaxTemp comparison", names.arg = c("1st cluster", "2nd cluster", "3rd cluster"), col = "dark blue")
barplot(k_results[,3], main = "Rainfall", names.arg = c("1st cluster", "2nd cluster", "3rd cluster"), col = "dark blue")
barplot(k_results[,6], main = "WindGustSpeed comparison", names.arg = c("1st cluster", "2nd cluster", "3rd cluster"), col = "dark blue")
barplot(k_results[,10], main = "Humidity3pm comparison", names.arg = c("1st cluster", "2nd cluster", "3rd cluster"), col = "dark blue")
barplot(k_results[,5], main = "Sunshine comparison", names.arg = c("1st cluster", "2nd cluster", "3rd cluster"), col = "dark blue")
