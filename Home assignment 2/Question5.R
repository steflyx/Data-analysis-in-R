#################################################################################
# ASSIGNMENT 2 - 5
# 
#################################################################################

#Packages
library(vegan)

#Introductory work
distances <- read.csv("./Dataset_distances/Distances.csv")
world_dist <- read.csv("./Dataset_distances/World_Distances.csv")

#################################################################################

#Question 1a)

#Rescale the distances
distances_mds <- cmdscale(distances, k = 10, eig = TRUE)

#We plot the scaled distances
plot(distances_mds$points, xlim = c(-2700,6200), ylim = c(-1000,1500), xlab = "Coordinate 1", ylab = "Coordinate 2", main = "MDS distances")
text(distances_mds$points[,1], distances_mds$points[,2], labels=c("MOSCOW", "STPETERSBURG", "YEKATERINBURG", "SAMARA", "VLADIVOSTOK", "OMSK", "KAZAN", "VOLGOGRAD", "KALININGRAD", "TULA", "MURMANSK"), cex= 0.7, pos=3)

##################################################################################

#Question 1b)

#Eigenvalues
eig <- distances_mds$eig

#Let's evaluate them
cumsum(abs(eig)) / sum(abs(eig))
cumsum(eig^2) / sum(eig^2)

#################################################################################

#Question 1c)

#Let's take the matrix of the distances
distance_matrix <- as.matrix(distances)

#Plot Shepard diagram
distance_sh <- Shepard(distances[lower.tri(distances)], distances_mds$points)
plot(distance_sh, pch = ".", xlab = "Dissimilarity", ylab = "Distance", xlim = range(distance_sh$x), ylim = range(distance_sh$x), main = "Shepard diagram")
lines(distance_sh$x, distance_sh$yf, type = "S")

################################################################################

#Question 1d)

#Let's check if the approximation is good in a high dimensional space
max(abs(dist(distances) - dist(cmdscale(dist(distances), k = 10))))
max(abs(dist(distances) - dist(cmdscale(dist(distances), k = 8))))
max(abs(dist(distances) - dist(cmdscale(dist(distances), k = 5))))

################################################################################

#Question 2a)

#Rescale the distances
world_mds <- cmdscale(world_dist, k = 9, eig = TRUE)

#We plot the scaled distances
plot(world_mds$points, xlab = "Coordinate 1", xlim = c(-11000,9000), ylim = c(-10000,5000), ylab = "Coordinate 2", main = "MDS distances")
text(world_mds$points[,1], world_mds$points[,2], labels=c("NEWYORK", "ROME", "MOSCOW", "CASABLANCA", "LONDON", "QUEENSTOWN", "TOKYO", "ISTANBUL", "OTTAWA", "SEATTLE"), cex= 0.7, pos=3)

################################################################################

#Question 2b)

#Eigenvalues
world_eig <- world_mds$eig

#Let's evaluate them
cumsum(abs(world_eig)) / sum(abs(world_eig))
cumsum(world_eig^2) / sum(world_eig^2)

####################################################################################

#Question 2c)

#Plot Shepard diagram
world_sh <- Shepard(world_dist[lower.tri(world_dist)], world_mds$points)
plot(world_sh, pch = ".", xlab = "Dissimilarity", ylab = "Distance", xlim = range(world_sh$x), ylim = range(world_sh$x), main = "Shepard diagram")
lines(world_sh$x, world_sh$yf, type = "S")

####################################################################################

#Question 2d)

#Let's check if the approximation is good in a high dimensional space
max(abs(dist(world_dist) - dist(cmdscale(dist(world_dist), k = 9))))
max(abs(dist(world_dist) - dist(cmdscale(dist(world_dist), k = 8))))
max(abs(dist(world_dist) - dist(cmdscale(dist(world_dist), k = 5))))
