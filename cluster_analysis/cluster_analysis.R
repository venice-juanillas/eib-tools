###########################################################################
## 
## File Name: 'Script.R'
##
## Authors:      JA Burgueno  < j.burgueno@cgiar.org >
##               FHRB Toledo  < f.toledo@cgiar.org >
##               VM Juanillas < v.juanillas@irri.org >
##
## Local / Date: CIMMYT, Mexico / Fri Apr 20th 2018
##
## Source:       SVD/PCA/Clustering for genotypic data
##
## Contents:     To be integrated to GOBBI/Galaxy
##
##            ... several -- read comments (# ...)
##
## `` Far better an approximate answer to the right question, which is 
## often vague, than the exact answer to the wrong question, which can
## always be made precise ''   (John Tukey, Ann. Math. Stat. [33] 1962)
##
###########################################################################

rm(list = objects()); ls() # CLEAR 'WorkSpace'             (R  environment)

## supply necessary libraries
library(mclust)      # clustering methods
library(ggplot2)     # gramar graphics
library(plotly)      # interactive plots
library(htmlwidgets) # saving interactive plots
library(optparse)

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file name", metavar="input_file"),
  make_option(c("-d", "--dimension"), type="integer", default=10, 
              help="number of dimensions to plot in barplot", metavar="num_dimensions"),
  make_option(c("-g", "--maxgroup"), type="integer", default=5, 
              help="maximum number of groups", metavar="maximum_groupings"),
  make_option(c("-m", "--membershiptable"), type="character", default="cluster_membership_file.txt", 
              help="Cluster membership file[default= %default]", metavar="membership_file"),
  make_option(c("-w", "--widgetgraph"), type="character", default="cluster_graph.html", 
              help="Graph File [default= %default]", metavar="graph_file")
); 
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$file)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}

## import data
data <- read.csv(file = opt$file, header = TRUE, stringsAsFactors = FALSE)

markers <- as.matrix(subset(data, select = -c(Name, Group))) # subset markers

SVD <- svd(markers) # peform SVD decomposition

percent <- SVD$d / sum(SVD$d) * 100 # dimensions contribution (%)

## [PARAMETER] number of dimensions
#dimensions <- 10
dimensions <- opt$dimension
barplot(percent[1:dimensions])

## combine scores and loadings
Gscores <- data.frame(Name = data$Name, Group = data$Group, SVD$u[, 1:dimensions],
                      stringsAsFactors = FALSE)

names(Gscores)[-c(1:2)] <- paste0("dim", 1:dimensions) # renaming properly

## interactive plot
igraph_genot <- ggplotly(p = ggplot(Gscores, aes(x = dim1, y = dim2, color = Group)) +
                           geom_point(mapping = aes(names = Name)))

## saving as html
saveWidget(igraph_genot, opt$widgetgraph, selfcontained = FALSE)

## how to know the 

## [PARAMETER] maxmimum number of groups
#maxg <- 20
maxg <- opt$maxgroup

## clustering
clusters <- Mclust(subset(Gscores, select = paste0("dim", 1:dimensions)), G = 1:maxg,
                   verbose = FALSE)

## collecting clustering results
classification <- data.frame(Name = Gscores$Name, Group = Gscores$Group,
                             Class = clusters$classification,
                             Uncertainty = round(clusters$uncertainty, 4),
                             stringsAsFactors = FALSE)

## saving clustering results
write.table(x = merge(classification, Gscores, by = c("Name", "Group")),
            file = opt$membershiptable, quote = FALSE, sep = '\t', eol = '\n',
            na = '.', dec = '.', row.names = FALSE, col.names = TRUE)

## \EOF
###########################################################################
