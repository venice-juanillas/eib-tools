# Umesh Rosyara, April 10, 2018
# CIMMYT
# Clustering method 2: Hierarchical Clustering and Cut the Tree
# this should run after finding optimal number of clustering 
################################################################
load("/Users/urosyara/Documents/gobii/genomocic_selection_pipeline/data2.rda")

#####################################################################
# Hierarchical Clustering and Cut the Tree

require(factoextra)
# Hierarchical Clustering and Cut the Tree
# hcut(x, k = 2, isdiss = inherits(x, "dist"), hc_func = c("hclust","agnes", "diana"), 
#     hc_method = "ward.D2", hc_metric = "euclidean",
#     stand = FALSE, graph = FALSE, ...)

# Need to wrap this function with the following options 

# x	a numeric matrix, numeric data frame or a dissimilarity matrix.
# k	 the number of clusters to be generated.
# isdiss	 logical value specifying wether x is a dissimilarity matrix.
# hc_func	the hierarchical clustering function to be used. Default value is "hclust". Possible values is one of "hclust", "agnes", "diana". Abbreviation is allowed.
# hc_method	the agglomeration method to be used (?hclust) for hclust() and agnes(): "ward.D", "ward.D2", "single", "complete", "average", ...
# hc_metric	character string specifying the metric to be used for calculating dissimilarities between observations. Allowed values are those accepted by the function dist() [including "euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski"] and correlation based distance measures ["pearson", "spearman" or "kendall"].
# stand	 logical value; default is FALSE. If TRUE, then the data will be standardized using the function scale(). Measurements are standardized for each variable (column), by subtracting the variable's mean value and dividing by the variable's standard deviation.
# graph	logical value. If TRUE, the dendrogram is displayed.

result <- hcut(data2, k = 3, hc_func="hclust",hc_method="ward.D",hc_metric="euclidean", 
               stand = TRUE, graph=TRUE)
# Cluster assignements of observations
result$cluster
# Size of clusters
result$size
# plot cluster results 
# Visualize the dendrogram
fviz_dend(result, rect = TRUE)
# Visualize clusters as scatter plots
fviz_cluster(result)
# Visualize the silhouette
fviz_silhouette(result)

# save the cluster information with data
data3 <-  data.frame ( data2, cluster = result$cluster) 

write.csv(data3, file = "data2hcut.csv", row.names = FALSE)








