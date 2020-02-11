#################################################################################
# ASSIGNMENT 2 - 1
# 
# DATASET: Civilian Unemployment Rate (https://fred.stlouisfed.org/series/UNRATE)
#################################################################################

#Introductory work

#Packages
library(quantmod)
library(normtest)
library(car)

#Dataset download
Unemployment_Rate <- getSymbols("UNRATE", src = "FRED", auto.assign = FALSE)
length(Unemployment_Rate)

#Delete missing values
Unemployment_Rate <- na.omit(Unemployment_Rate)
length(Unemployment_Rate)

##############################################################################
# EXERCISE 1

#Question a)

#Find an a 97% confidence interval for the mean
t.test(Unemployment_Rate, conf.level = 0.97)

##############################################################################

#Question b)

#Median
median <- median(Unemployment_Rate)

#Check p-value for mean = median
t.test(Unemployment_Rate, mu = median)

###############################################################################

#Question c)

#Standard deviation of the sample
Sample_Deviation <- sd(Unemployment_Rate)

#X2 values
alpha = .05 #95% confidence interval
lstar = qchisq(alpha/2, df = length(Unemployment_Rate)-1)
rstar = qchisq(1-alpha/2, df = length(Unemployment_Rate)-1)

#Standard deviation
sqrt((length(Unemployment_Rate)-1)*Sample_Deviation^2 * c(1/rstar,1/lstar))

###############################################################################

#Question d-e)

#Let's select a proportion of the data. I chose all rows where unemployment > 6%
length_unemployment_more_6 <- length(Unemployment_Rate[which(Unemployment_Rate > 6),1])

#Let's calculate the confidence interval for the proportion of unemployment > 6%
prop.test(length_unemployment_more_6, length(Unemployment_Rate), conf.level = 0.99)

###############################################################################

#Question f-g)

#We split up the dataset in two (before/after 1980)
Unemployment_before_1980 <- Unemployment_Rate['/1980']
Unemployment_after_1980 <- Unemployment_Rate['1981/']
length_bef <- length(Unemployment_before_1980)
length_aft <- length(Unemployment_after_1980)

#We use the same condition (UNRATE > 6%)
over_before <- length(Unemployment_before_1980[which(Unemployment_before_1980 > 6)])
over_after <- length(Unemployment_after_1980[which(Unemployment_after_1980 > 6)])

#We calculate the confidence intervals
prop.test(c(over_after, over_before), c(length_aft, length_bef), conf.level = 0.99)

###############################################################################

#EXERCISE 2

#Introductory work

#First dataset: coffee price from FRED (monthly)
coffee <- getSymbols("PCOFFOTMUSDM", src = "FRED", auto.assign = FALSE)
coffee <- na.omit(coffee)
coffee$log_returns <- diff(log(coffee), lag = 1)
coffee <- coffee[,2]
coffee <- na.omit(coffee)

#Second dataset: sugar price from FRED (monthly)
sugar <- getSymbols("PSUGAISAUSDM", src = "FRED", auto.assign = FALSE)
sugar <- na.omit(sugar)
sugar$log_returns <- diff(log(sugar), lag = 1)
sugar <- sugar[,2]
sugar <- na.omit(sugar)

#Third dataset: cotton price from FRED (monthly)
cotton <- getSymbols("PCOTTINDUSDM", src = "FRED", auto.assign = FALSE)
cotton <- na.omit(cotton)
cotton$log_returns <- diff(log(cotton), lag = 1)
cotton <- cotton[,2]
cotton <- na.omit(cotton)

###########################################################################

#Question a)

#We perform the Jarque-Bera test using the 'normtest' package
jb.norm.test(coffee)
jb.norm.test(sugar)
jb.norm.test(cotton)

###########################################################################

#Question b)

#Let's build the qqPlot
qqPlot(coredata(coffee), main = "QQ-Plot coffee", ylab = "Coffee returns")
qqPlot(coredata(sugar), main = "QQ-Plot sugar", ylab = "Sugar returns")
qqPlot(coredata(cotton), main = "QQ-Plot cotton", ylab = "Cotton returns")

###########################################################################

#EXERCISE 3

#Introductory work
crabs <- MASS::crabs

#Let's perform the chi-squared test
chisq.test(crabs[1:50,]$FL, p = rep(1/50, 50))

###########################################################################

#EXERCISE 4

#Introductory work

#We count male and female with front lobe more than 14
num_female_more_14 <- length(crabs[which(crabs$sex == "F" & crabs$FL > 14),1])
num_female_less_14 <- length(crabs[which(crabs$sex == "F" & !crabs$FL > 14),1])
num_male_more_14 <- length(crabs[which(crabs$sex == "M" & crabs$FL > 14),1])
num_male_less_14 <- length(crabs[which(crabs$sex == "M" & !crabs$FL > 14),1])

#We build the matrix
crabs_matrix <- rbind(c(num_female_more_14, num_female_less_14), c(num_male_more_14, num_male_less_14))

#Chi-squared test
chisq.test(crabs_matrix)
