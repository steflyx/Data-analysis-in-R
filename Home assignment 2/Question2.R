#################################################################################
# ASSIGNMENT 2 - 2
# 
# DATASET: Crabs from MASS
#################################################################################

#Introductory work

#Packages
library(quantmod)
library(MASS)
library(sm)
library(ggplot2)
library(class)
library(normtest)
library(car)

#Introductory work
crabs <- MASS::crabs

###############################################################################

#Question 1a)

#Compute lm
stats = lm(CW ~ CL, data = crabs)

#Plot the result
plot(y = crabs$CW, x = crabs$CL, pch = 16, cex = .7, col = "blue", main = "BODY LENGTH AGAINST BODY WIDTH", xlab = "BODY LENGTH", ylab = "BODY WIDTH")
abline(stats)

#############################################################################

#Question 1b)

#Let's see the various statistics of our model
summary(stats)

##################################################################################

#Question 1c)

#Let's plot the fitted values against the residuals (line at 0 is used to interpret results)
plot(fitted(stats), resid(stats), col = "blue", pch = 20, main = "RESIDUALS AGAINST FITTED VALUES", ylab = "Residuals", xlab = "Fitted")
abline(h = 0, lty = c(2), col = "red", lwd = 2)

#Let's observe the qqplot
qqPlot(resid(stats), main = "QQ-Plot residuals", ylab = "Residuals")

#############################################################################

#Question 1d)

#Let's generate new values
new_CL_values <- runif(10, min = min(crabs$CL), max = max(crabs$CL))
new_values <- data.frame(CL = c(new_CL_values))

#Let's make the predictions
forecasts <- predict(stats, newdata = new_values, interval = "pred")
forecasts_mean <- predict(stats, newdata = new_values, interval = "confidence")
prediction_results <- data.frame(CW_predicted = c(forecasts[,1]), CL = new_CL_values)

#FIRST PLOT: ONLY POINTS
plot(y = crabs$CW, x = crabs$CL, pch = 16, cex = .5, col = "blue", main = "BODY LENGTH AGAINST BODY WIDTH WITH PREDICTIONS", xlab = "BODY LENGTH", ylab = "BODY WIDTH")
points(x = prediction_results[,2], y = prediction_results[,1], col = "red", pch = 16, cex = 1.3)
abline(stats)
legend(35,30,c("Observations","Predictions"), pch = c(16,16), col=c("blue","red"), y.intersp=1.5)


#SECOND PLOT: CONFIDENCE INTERVAL FULL DIMENSION
#Let's plot the points and the model line
plot(y = crabs$CW, x = crabs$CL, pch = 16, cex = .5, col = "blue", main = "BODY LENGTH AGAINST BODY WIDTH WITH PREDICTIONS", xlab = "BODY LENGTH", ylab = "BODY WIDTH")
points(x = prediction_results[,2], y = prediction_results[,1], col = "red", pch = 16, cex = 1.3)
abline(stats)

#Let's plot the single value interval
lines(x = prediction_results[,2], y = forecasts[,2], col = "red", lty = "dotted", lwd = "2")
lines(x = prediction_results[,2], y = forecasts[,3], col = "red", lty = "dotted", lwd = "2")

#Let's plot the mean value
lines(x = prediction_results[,2], y = forecasts_mean[,2], col = "grey", lwd = "2")
lines(x = prediction_results[,2], y = forecasts_mean[,3], col = "grey", lwd = "2")

legend(30,30,c("Single value interval","Mean value interval"), lwd=c(2,2), lty = c(3,1),  col=c("red","grey"), y.intersp=1.5)



#THIRD PLOT: CLOSE UP
#Let's plot the points and the model line
plot(y = crabs$CW, x = crabs$CL, pch = 16, cex = .5, col = "blue", main = "BODY LENGTH AGAINST BODY WIDTH WITH PREDICTIONS", xlab = "BODY LENGTH", ylab = "BODY WIDTH", xlim = c(25,35))
points(x = prediction_results[,2], y = prediction_results[,1], col = "red", pch = 16, cex = 1.3)
abline(stats)

#Let's plot the single value interval
lines(x = prediction_results[,2], y = forecasts[,2], col = "red", lty = "dotted", lwd = "2")
lines(x = prediction_results[,2], y = forecasts[,3], col = "red", lty = "dotted", lwd = "2")

#Let's plot the mean value
lines(x = prediction_results[,2], y = forecasts_mean[,2], col = "grey", lwd = "2")
lines(x = prediction_results[,2], y = forecasts_mean[,3], col = "grey", lwd = "2")

legend(30,30,c("Single value interval","Mean value interval"), lwd=c(2,2), lty = c(3,1),  col=c("red","grey"), y.intersp=1.5)

###############################################################################

#Question 2

#Introductory work

#Let's create a categorical variable for the crabs' sex
crabs$sex_num <- 0
crabs$sex_num[crabs$sex == "M"] <- 1

############################################################################

#Question 2b-c)

#Let's create a multiple variable model
stats = lm(CW ~ CL + RW + FL, data = crabs)
summary(stats)

###############################################################################

#Question 2d)

#Plot fitted vs residuals
plot(fitted(stats), resid(stats), col = "blue", pch = 20, main = "RESIDUALS AGAINST FITTED VALUES", ylab = "Residuals", xlab = "Fitted")
abline(h = 0, lty = c(2), col = "red", lwd = 2)

###########################################################################

#Question 2e)

#1-variable model

#Only FL
stats = lm(CW ~ FL, data = crabs)
summary(stats)

#Only RW
stats = lm(CW ~ RW, data = crabs)
summary(stats)

#Only BD (Body Depth)
stats = lm(CW ~ BD, data = crabs)
summary(stats)

#2-variable model

#CL + sex
stats = lm(CW ~ CL + sex_num, data = crabs)
summary(stats)

#CL + FL
stats = lm(CW ~ CL + FL, data = crabs)
summary(stats)

#CL + BD
stats = lm(CW ~ CL + BD, data = crabs)
summary(stats)

#FL + BD
stats = lm(CW ~ FL + BD, data = crabs)
summary(stats)

#3-variable model

#CL + RW + BD
stats = lm(CW ~ CL + RW + BD, data = crabs)
summary(stats)

#CL + FL + BD
stats = lm(CW ~ CL + FL + BD, data = crabs)
summary(stats)

#5-variable model

#CL + FL + BD + RW + sex
stats = lm(CW ~ CL + FL + BD + RW + sex_num, data = crabs)
summary(stats)

##################################################################################################

#Question 3

#Question 3a)

#Let's build the logistic regression model
stats = glm(sex_num ~ CL, data = crabs, family = binomial)
summary(stats)

##################################################################################################

#Question 3b)

#We use stepAic function to find the best model
Model_glm <- glm(sex_num ~ CL + FL + BD + RW + CW, data = crabs, family = binomial)
best_model <- stepAIC(Model_glm, direction = 'backward')

##################################################################################################

#Question 3c)

#Let's recover the data
sex_num <- crabs$sex_num
CL <- crabs$CL
FL <- crabs$FL
RW <- crabs$RW

#Let's generate new variables
new_CL <- runif(1, min = min(CL), max = max(CL))
new_FL <- runif(1, min = min(FL), max = max(FL))
new_RW <- runif(1, min = min(RW), max = max(RW))
new_var <- data.frame(CL = c(new_CL), FL = c(new_FL), RW = c(new_RW))

#Let's make the prediction
lm_model <- glm(sex_num ~ CL + FL + RW, family = binomial)
predict(lm_model, new = data.frame(new_var), type = "response")

#Let's plot the distributions to evaluate the results
ggplot(data = crabs, aes(CL, fill = sex)) + geom_density(alpha = 0.2) + ggtitle("Crabs' length compared by sex") + xlab("Crabs' length")
ggplot(data = crabs, aes(FL, fill = sex)) + geom_density(alpha = 0.2) + ggtitle("Crabs' front lobe compared by sex") + xlab("Crabs' front lobe")
ggplot(data = crabs, aes(RW, fill = sex)) + geom_density(alpha = 0.2) + ggtitle("Crabs' rear width compared by sex") + xlab("Crabs' rear width")

##################################################################################################

#Question 3d)

#Let's split the dataset (75-25) - source code: https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
smp_size <- floor(0.75 * nrow(crabs))
set.seed(123)
train_ind <- sample(seq_len(nrow(crabs)), size = smp_size)
train <- crabs[train_ind, ]
test <- crabs[-train_ind, ]

#Let's rebuild the model
new_model <- glm(sex_num ~ CL + FL + RW, family = binomial)
test_var <- data.frame(CL = c(test$CL), FL = c(test$FL), RW = c(test$RW))
predictions <- predict(new_model, new = data.frame(test_var), type = "response")

#Let's turn the predictions into 0 and 1
threshold = 0.5
for (i in 1:50){
  if(predictions[i] < threshold){
    predictions[i] = 0
  }else {
    predictions[i] = 1
  }
}

#Let's build a dataframe with the comparison
comparison_prediction_test <- data.frame(predictions = c(predictions), actual_data = c(test$sex_num))

#Let's count the correct predictions
correct_predictions <- nrow(comparison_prediction_test[comparison_prediction_test$predictions == comparison_prediction_test$actual_data,])
percentage <- correct_predictions / nrow(comparison_prediction_test) * 100

############################################################################################################

#Question 4a)

#Let's use lda
train_rows <- sample(1:200, 150)
LDA_results <- lda(formula = sex_num ~ CL + CW + RW + FL + BD, crabs, subset = train_rows, prior = c(1,1)/2)
plot(LDA_results)

#Let's make the predictions
LDA_predictions <- predict(object = LDA_results, newdata = crabs[-train_rows,])

#Let's compare
LDA_predictions_comparison <- data.frame(predictions = c(LDA_predictions$class) - 1, actual_data = c(crabs[-train_rows,]$sex_num))
LDA_correct_predictions <- nrow(LDA_predictions_comparison[LDA_predictions_comparison$predictions == LDA_predictions_comparison$actual_data,])
LDA_correct_predictions_percentage <- (LDA_correct_predictions / nrow(LDA_predictions_comparison) * 100)

########################################################################################################

#Question 4b)

#Let's use the qda command
QDA_results <- qda(formula = sex_num ~ CL + CW + RW + FL + BD, data = crabs)

######################################################################################################

#Question 5a)

#Normalization
nor <-function(x) {(x -min(x))/(max(x)-min(x))}
crabs_norm <- as.data.frame(lapply(crabs[,c(4,5,6,7,8)], nor))

#Split in train and test set
crabs_norm_train <- crabs_norm[train_rows,]
crabs_norm_test <- crabs_norm[-train_rows,]

#Target category
crabs_target_train <- crabs[train_rows, 9]
crabs_target_test <- crabs[-train_rows, 9]

#Let's use the knn command
KNN_results <- knn(train = crabs_norm_train, test = crabs_norm_test, cl = crabs_target_train)

#Comparison
KNN_comparison <- data.frame(KNN_predictions = c(KNN_results)-1, actual_data = c(crabs_target_test))
KNN_correct_predictions <- nrow(KNN_comparison[KNN_comparison$KNN_predictions == KNN_comparison$actual_data,])

######################################################################################################

#Question 5b)

#Let's repeat the knn classification
K_neighbors = c(1, 2, 5, 8, 10, 20, 30, 50, 75, 100, 150)
for (i in 1:11){
  
  #KNN classification
  KNN_results <- knn(train = crabs_norm_train, test = crabs_norm_test, cl = crabs_target_train, k = K_neighbors[i])
  
  #Comparison
  KNN_comparison <- data.frame(KNN_predictions = c(KNN_results)-1, actual_data = c(crabs_target_test))
  KNN_correct_predictions <- nrow(KNN_comparison[KNN_comparison$KNN_predictions == KNN_comparison$actual_data,])
  
  print("K-neighbors:")
  print(K_neighbors[i])
  print("SUccess rate:")
  print(KNN_correct_predictions)
  print(" ")
}