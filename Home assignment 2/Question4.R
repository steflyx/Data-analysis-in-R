#################################################################################
# ASSIGNMENT 2 - 4
# 
# DATASET: Peron election results (http://users.stat.ufl.edu/~winner/data/peron.txt))
#################################################################################

#Packages
library(FactoMineR)
library(MASS)
library(factoextra)

#Introductory work
peron <- read.csv("./Dataset_peron/Peron.csv")

########################################################################################

#Question 2a)

#We perform the X2 test
chisq.test(peron)

#########################################################################################

#Question 2b-c)

#Compute the CA
CA_results <- CA(peron[2:4], graph = FALSE)

#Plot rows and columns separately
fviz_ca_row(CA_results)
fviz_ca_col(CA_results)

#Plot them together
fviz_ca_biplot(CA_results)

############################################################################################

#Question 2d)

#Compute eigenvalues
get_eigenvalue(CA_results)

#Visualize them
fviz_eig(CA_results)

################################################################################################

#Question 2e)

#Let's observe cos2 for rows and columns
CA_results$col$cos2
CA_results$row$cos2
