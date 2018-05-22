#######################################################################
# Umesh Rosyara, April 10, 2018
# CIMMYT
# optimum QTL: This function is used to do find optimum number of QTLs
######################################################################


rm(list = objects()); ls() # CLEAR 'WorkSpace'             (R  environment)

library(optparse)

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file name", metavar="input_file"),
  make_option(c("-m", "--method"), type="character", default="silhouette", 
              help="the method to be used for estimating the optimal number of clusters", metavar="method "),
  make_option(c("-k", "--maxclust"), type="integer", default=2, 
              help=" the maximum number of clusters to consider, must be at least 2", metavar="num of clusters "),
  make_option(c("-b", "--nboot"), type="integer", default=200, 
              help="number of Monte Carlo (bootstrap) samples", metavar="num of groups "),
  make_option(c("-c", "--clusterfile"), type="character", default="cluster_file.txt", 
              help="Cluster membership file[default= %default]", metavar="membership_file"),
  make_option(c("-g", "--graph"), type="character", default="graph.html", 
              help="Graph File [default= %default]", metavar="graph_file")
); 
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$file)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

data1 <- read.csv(file = opt$file, header = TRUE, stringsAsFactors = FALSE)
#data1 <- t(data)
data1c <-   data1[,-1] 

# PCA /cluster analysis works on numeric variables, so we need to code the AA=1, AB=0, BB=-1 or similar numerical encoding 

# imputation of missing values 
# this example is just imputing with mean, but the final should have better imputing algorithm 
# I think that was already in place at IRRI hackathan 
#impute with population mean
data1i <- apply(data1c[,-1],1,function(x){ix <- which(is.na(x)); x[ix] <- mean(x,na.rm=T); return(x)})

# data with no missing value 
data2 <- t(data1i)
rownames(data2) <- data1[,1]


##################################################################
# Finding optimum number of clusters 
#Determining Optimal Clusters
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

set.seed(1234)
# we need to wrap the function fviz_nbclust
#fviz_nbclust(): Dertemines and visualize the optimal number of clusters using different methods: within cluster sums of squares, average silhouette and gap statistics.

#fviz_nbclust(x, FUNcluster = NULL, method = c("silhouette", "wss","gap_stat"), diss = NULL, k.max = 10, nboot = 100,
#             verbose = interactive(), barfill = "steelblue", barcolor = "steelblue",
#             linecolor = "steelblue", print.summary = TRUE, ...)

#User needs to choose the following options
# x - data set 
# FUNcluster - allowed values: kmeans, cluster::pam, cluster::clara, cluster::fanny, hcut
# method - the method to be used for estimating the optimal number of clusters. Possible values are "silhouette" (for average silhouette width), "wss" (for total within sum of square) and "gap_stat" (for gap statistics
# k.max - the maximum number of clusters to consider, must be at least 2 
# nboot - integer, number of Monte Carlo ("bootstrap") samples. Used only for determining the number of clusters using gap statistic.

# Usage three different methods 
pdf(opt$graph)
if(opt$method == "silhouette"){
  output1 <- fviz_nbclust(data2, FUNcluster=kmeans, method = "silhouette", k.max = opt$maxclust)

 
}else if(opt$method == "gap_stat"){
  output1 <- fviz_nbclust(data2, FUNcluster=kmeans, method = "gap_stat",k.max = opt$maxclust, nboot = opt$nboot)
}else if(opt$method == "wss"){
  output1 <- fviz_nbclust(data2, FUNcluster=kmeans, method ="wss", k.max = opt$maxclust)
}
print(output1)
dev.off()

