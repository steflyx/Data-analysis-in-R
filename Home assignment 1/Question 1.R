##################################################################################################
# ASSIGNMENT 1 - 1
#
# Dataset: 10-Year Treasury Constant Maturity Rate (Daily)
##################################################################################################

#INTRODUCTURY WORK

#Packages
library(quantmod)
library(ggplot2)

#Dataset download from St Louis Fred
Ten_Year <- getSymbols("DGS10", src = "FRED", auto.assign = FALSE)
length(Ten_Year)

#Check for missing values
missing <- Ten_Year[which(is.na(Ten_Year))]
length(missing)

#We delete missing values
Ten_Year <- na.omit(Ten_Year)

####################################################################################################

#Question a)

#Stem and leaf
stem(Ten_Year)

#Histogram
ggplot(Ten_Year, aes(DGS10)) + geom_histogram(aes(y=..density..), color = 'black', fill = 'white', bins = 100) + geom_density(color = 'red', size = 1) + ylab("Density") + xlab("10Y Interest Rates")

####################################################################################################

#Question b)

#Mean and median
mean <- mean(Ten_Year)
median <- median(Ten_Year)

#Portion of data less than mean
length(Ten_Year[Ten_Year < mean]) / length(Ten_Year) * 100

#####################################################################################################

#Question c)

#1st and 3rd Quartile
quantile(Ten_Year, 0.25)
quantile(Ten_Year, 0.75)

#90th quantile
quantile(Ten_Year, 0.9)

#Mode
get_mode <- function(dataset){
  
  dataset_unique <- unique(dataset)
  max_occurences <- 0
  mode <- 0
  for (i in dataset_unique){
    len = length(Ten_Year[Ten_Year == i])
    if (len > max_occurences){
      max_occurences = len
      mode = i
    }
  }
  
  return(mode)
  
}

get_mode(Ten_Year)

############################################################################################

#Question d)

#Range
diff(range(Ten_Year))

#Sample Standard Deviation
sd(Ten_Year)

#IQR
IQR(Ten_Year)

#Box Plot
out = boxplot(coredata(Ten_Year), ylab = "Interest Rate", main = "Bonds since 1962", col = "red")

#Outliers Analysis
out$out
length(out$out)
min(out$out)

#####################################################################################################

#Question e)

#QQ-Plot
qqnorm(coredata(Ten_Year), main='Interest Rates')
qqline(coredata(Ten_Year))
