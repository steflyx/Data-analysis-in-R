##############################################################################################################
# ASSIGNMENT 1 - 3
#
# Dataset: 2012 SAT results in New York (https://catalog.data.gov/dataset/sat-results-e88d7)
##############################################################################################################

#INTRODUCTURY WORK

#Packages
library(quantmod)
library(ggplot2)

#Dataset from Data.gov concerning popular baby names in the US
SAT_Result <- read.csv('2012_SAT_Results.csv', stringsAsFactors = FALSE)
length(SAT_Result[, 1])
str(SAT_Result)

#For the goal of this exercise it's useful to consider only a portion of the data
SAT_Result <- SAT_Result[0:50, 4:6]

#Transforms data from chr to numric
SAT_Result$ReadingScore <- as.numeric(as.character(SAT_Result$ReadingScore))
SAT_Result$MathScore <- as.numeric(as.character(SAT_Result$MathScore))
SAT_Result$WritingScore <- as.numeric(as.character(SAT_Result$WritingScore))

#We eliminate NA values
SAT_Result <- na.omit(SAT_Result)
length(SAT_Result[, 1])

##############################################################################################################

#Question a)

#Bubble plot
ylim <- with(SAT_Result, range(SAT_Result$ReadingScore)) * c(0.95, 1)
plot(SAT_Result$ReadingScore, SAT_Result$MathScore, xlab = "Average Reading Score", ylab = "Average Math Score", pch = 10, ylim = ylim, main = "Bubble Plot")
with(SAT_Result, symbols(ReadingScore, MathScore, circles = WritingScore, inches = 0.5, add = TRUE))

##############################################################################################################

#Question b)

#Glyph plot
stars(SAT_Result, cex = 0.55, main = "Glyph Plot")

##############################################################################################################

#Question c)

#Scatter plot matrix
pairs(SAT_Result[,1:3], pch = 19, main = "Scatter Plot Matrix")
