# Umesh Rosyara, April 10, 2018
# CIMMYT
# Clustering method 1: K-means 
# this should run after finding optimal number of clustering 
################################################################
load("/Users/urosyara/Documents/gobii/genomocic_selection_pipeline/data2.rda")

#####################################################################
### K mean clustering #################################################
#######################################################################

# kmeans: Perform k-means clustering on a data matrix.
# kmeans(x, centers, iter.max = 10, nstart = 1,
#       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
#                     "MacQueen"), trace=FALSE)

# x	numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
# centers	 either the number of clusters, say k, or a set of initial (distinct) cluster centres. If a number, a random set of (distinct) rows in x is chosen as the initial centres.
# iter.max	the maximum number of iterations allowed.
# nstart	if centers is a number, how many random sets should be chosen?
# algorithm	character: may be abbreviated. Note that "Lloyd" and "Forgy" are alternative names for one algorithm.

results <- kmeans(data2, centers=3, iter.max = 10, nstart = 1, algorithm = "Hartigan-Wong")

# Visualize the kmeans final clusters
fviz_cluster(results, data = data2, ellipse.type = "norm", ellipse.level = 0.2)

# save the cluster information with data
data3 <- data.frame (data2, cluster = results$cluster)  
write.csv(data2, file = "data2kmean.csv", row.names = FALSE)









