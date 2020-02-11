#################################################################################
# ASSIGNMENT 2 - 3
# 
# DATASET: Red wine quality (https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009/download)
#################################################################################

#Packages
library(FactoMineR)
library(ggplot2)
library(factoextra)

#Introductory work
wine_quality <- read.csv("./Dataset_wine_quality/winequality-red.csv")
wine_quality <- wine_quality[1:100,]

#We introduce a categorical variable (1 if quality >5, 0 otherwise)
wine_quality[wine_quality$quality <= 5,]$quality <- 0
wine_quality[wine_quality$quality > 0,]$quality <- 1

#######################################################################################################

#Question 2a)

#We find the PCs
wine.pca <- prcomp(wine_quality[,1:11], center = TRUE,scale. = TRUE)

#We plot them
plot(wine.pca$x[,1],wine.pca$x[,2], xlab="PC1", ylab = "PC2", main = "PC1 / PC2 - scatterplot")
biplot(wine.pca, scale=0, main = "PC1 / PC2 - plot")

#######################################################################################################à

#Question 2b)

#Eigenvalues
wine.pca.var <- wine.pca$sdev^2
eigenvalues <- wine.pca.var/sum(wine.pca.var)

#Plotting them
plot(eigenvalues, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b',col="blue")
lines(cumsum(eigenvalues), type = "b", col = "red")
title("PC/Variance explained")
legend(4.5,0.5,c("cumulative variance","variance"), lwd=c(1,1), col=c("red","blue"), y.intersp=1.5)

########################################################################################################

#Question 2c)

#This time we use the PCA function
pca_results <- PCA(wine_quality[,1:11], graph = FALSE)

#We print the cos2 results
pca_results$ind$cos2

#We plot them
hist(pca_results$ind$cos2[,1], breaks = 50, xlab = "Cos2 for dimension 1", main = "Distribution dimension 1", xlim = c(0,1))
hist(pca_results$ind$cos2[,2], breaks = 50, xlab = "Cos2 for dimension 2", main = "Distribution dimension 2", xlim = c(0,1))
hist(pca_results$ind$cos2[,3], breaks = 50, xlab = "Cos2 for dimension 3", main = "Distribution dimension 3", xlim = c(0,1))
hist(pca_results$ind$cos2[,4], breaks = 50, xlab = "Cos2 for dimension 4", main = "Distribution dimension 4", xlim = c(0,1))
hist(pca_results$ind$cos2[,5], breaks = 50, xlab = "Cos2 for dimension 5", main = "Distribution dimension 5", xlim = c(0,1))

###################################################################################################

#Question 2d)

#Let's make the plot
wine_quality$quality <- as.factor(wine_quality$quality)
fviz_pca_ind(pca_results, geom.ind = "point", col.ind = wine_quality$quality, palette = c("#00AFBB", "#E7B800"), addEllipses = TRUE, legend.title = "Groups")

############################################################################################################

#Question 3a)

#Plot to study the variables
fviz_pca_var(pca_results)

#####################################################################################################

#Question 3b)

#Let's print the cos2 for variables
pca_results$var$cos2

#######################################################################################################

#Question 3c)

#We use the pairs function
pairs(wine_quality[,c(2,9,11)])
pairs(wine_quality[,c(4,6,7)])
pairs(wine_quality[,c(1,3,5,8,10)])
