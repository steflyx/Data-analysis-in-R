##############################################################################################################
# ASSIGNMENT 1 - 2
#
# Dataset: 10-Year Treasury Constant Maturity Rate (Daily) and 2-Year Treasury Constant Maturity Rate (Daily)
##############################################################################################################

#INTRODUCTURY WORK

#Packages
library(quantmod)
library(ggplot2)
library(lubridate)
library(MVA)
library(gridExtra)

#Dataset download from St Louis Fred (Initial date is moved afterwards, since the second dataset
#is only available from 1976)
Ten_Year <- getSymbols("DGS10", src = "FRED", auto.assign = FALSE)
Two_Year <- getSymbols("DGS2",  src = "FRED", auto.assign = FALSE)
Ten_Year <- Ten_Year['1976-06-01/']
length(Ten_Year)
length(Two_Year)

#Check for missing values
missing_Ten <- Ten_Year[which(is.na(Ten_Year))]
missing_Two <- Two_Year[which(is.na(Two_Year))]
length(missing_Ten)
length(missing_Two)

#We delete missing values
Ten_Year <- na.omit(Ten_Year)
Two_Year <- na.omit(Two_Year)

##############################################################################################################

#Question a)

#Side by side boxplot
boxplot(c(coredata(Ten_Year)), c(coredata(Two_Year)), names = c("10Y Interest", "2Y Interest"), main = "Side by side boxplot")

##############################################################################################################

#Question b)

#Merge the two datasets
merged_rates <- merge(Ten_Year, Two_Year)
merged_rates$Year <- year(time(Ten_Year))

#Scatter plot
ggplot(merged_rates, aes(Ten_Year, Two_Year, color = Year)) + geom_point(size = 0.1) + xlab("10Y Interest Rate") + ylab("2Y Interest Rate")

##############################################################################################################

#Question c)

#Pearson's and SPearman's coefficient
cor(Ten_Year, Two_Year, method="pearson")
cor(Ten_Year, Two_Year, method="spearman")

##############################################################################################################

#Question d)

#Scatterplot with marginal distribution

#Scatter Plot
scatter <- ggplot(merged_rates, aes(Ten_Year, Two_Year, color = Year))+geom_point(size = 0.1) +  xlab("10Y interest rates") + ylab("2Y interest rates")

#Histogram
hist_top <- ggplot(merged_rates, aes(Ten_Year))+geom_histogram(color = "black", fill = "white") + xlab("10Y interest rates") + ylab(" ")

#Boxplot (we need a dummy variable for ggplot boxplot)
merged_rates$dummyVariable <- 1
boxplot_right <- ggplot(merged_rates, aes(x=dummyVariable, y=Two_Year)) + geom_boxplot() + ylab("2Y interest rates") + xlab(" ")

#Empty space between graphs
empty <- ggplot()+geom_point(aes(1,1), colour="white")+theme(axis.ticks=element_blank(),panel.background=element_blank(),axis.text.x=element_blank(), axis.text.y=element_blank(),axis.title.x=element_blank(), axis.title.y=element_blank())

#Grid
grid.arrange(grobs = list(hist_top, empty, scatter, boxplot_right), ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4), top = "Scatter Plot with Marginal Distributions")

###############################################################################################################

#Question e)

#Bivariate boxplot
x <- merged_rates[, c("DGS10", "DGS2")]
bvbox(coredata(x), main = "Bivariate Boxplot", xlab = "10-Years Interest Rates", ylab = "2-Years Interest Rates")

##############################################################################################################

#Question f)

#Convex hull

#Create a matrix with the data
X <- matrix(c(Ten_Year, Two_Year), ncol = 2)

#Compute the hull
hull <- chull(X)
hull <- c(hull, hull[1])

#Plot the convex hull
plot(X, cex = 0.5, xlab = "10Y interest rates", ylab = "2Y interest rates", main = "Convex hull")
lines(X[hull,])

#Recompute correlation coefficient
cor(Ten_Year[-hull], Two_Year[-hull])
