# Umesh Rosyara, April 10, 2018
# CIMMYT
# PCA.visualize: This function is used to do PCA analysis together with visualization of results in 2D or 3D 
# dummmy data 
#load("/Users/urosyara/Documents/gobii/genomocic_selection_pipeline/data2.rda")

rm(list = objects()); ls() # CLEAR 'WorkSpace'             (R  environment)

library(optparse)

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL,
              help="dataset file name", metavar="input_file"),
  make_option(c("-d", "--dmetric"), type="character", default="euclidean",
              help="the distance measure to be used", metavar="distance_metric "),
  make_option(c("-a", "--algorithm"), type="character", default="Hartigan-Wong",
              help="the algorithm to be used for kmeans", metavar="algorithm "),
  make_option(c("-m", "--method"), type="character", default="ward.D",
              help="the agglomeration method to be used", metavar="method "),
  make_option(c("-k", "--kcluster"), type="integer", default=2,
              help=" the number of clusters to be generated", metavar="num of clusters to generate"),
  make_option(c("-i", "--iter"), type="integer", default=10,
              help="the maximum number of iterations allowed for k-means", metavar="iterations "),
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


##################################################################
# PCA and initial visualization 
PCA.visualize <- function (data, ...){
  # data must have samples in column 
  # markers in rows
  data <- t(data)
  out1a <- prcomp(data, ...)
  # plot 
  par(mfrow=c(3,1))
  # variance explained by each components 
  varianceexp <- data.frame (numbercom = 1: length (out1a$sdev), 
                             varExp = cumsum((out1a$sdev)^2) / sum(out1a$sdev^2), 
                             IdvarExp = (out1a$sdev)^2 / sum(out1a$sdev^2)) 
  # plot variance explained by each components 
  plot(varianceexp$numbercom, varianceexp$varExp, type="b", col="blue2", pch=19, xlab="Components", 
       ylab = "Variance explained", main = "Cumulative variance explained")
  grid (NULL,NULL, lty = 6, col = "cornsilk2") 
  
  #plot two axis 
  plot(out1a$rotation[,1], y=out1a$rotation[,2], xlab=paste("PC1(", round(varianceexp[1,3],3)*100, "%)", sep=""), 
       ylab=paste("PC2(", round(varianceexp[2,3],3)*100, "%)", sep=""), 
       pch=19, col="blue2")
  grid (NULL,NULL, lty = 6, col = "cornsilk2") 
  
  # plot first three components 
  require(scatterplot3d)
  scatterplot3d(x=out1a$rotation[,1], y=out1a$rotation[,2], z=out1a$rotation[,3], 
                pch=19, color = "blue2",
                main=NULL, sub=NULL, xlim=NULL, ylim=NULL, zlim=NULL,
                xlab=paste("PC1(", round(varianceexp[1,3],3)*100, "%)", sep=""), ylab=paste("PC2(", round(varianceexp[2,3],3)*100, "%)", sep=""),
                zlab=paste("PC3(", round(varianceexp[3,3],3)*100, "%)", sep=""), scale.y=1, angle=45)
  par(mfrow=c(1,1))
  output <- out1a
  output$varianceexp <- varianceexp
  return(output) 
  
}

# data a numeric data matrix, data must have samples in row 
## ... arguments passed to prcomp 

PCAresults <- PCA.visualize(data1)

# save R object 
#save(PCAresults, file = "PCAresults.rda")

# saving PCA scores for future use 
write.csv(PCAresults$rotation, file = "PCAscores.csv")

# the plots should be displayed. 






