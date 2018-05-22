#########################################################
#                                                       #
# April  17, 2018                                       #
# Singular Value Decomposition of Genetic matrix        #
# Cluster analysis                                      #
#                                                       #
#########################################################

list.of.packages <- c("ggplot2","plotly","mclust","optparse","htmlwidgets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(plotly)
library(mclust)
library(optparse)
library(htmlwidgets)

rm(list = ls())

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file name", metavar="input_file"),
  make_option(c("-a", "--a"), type="integer", default=10, 
              help="number of dimensions to plot in barplot", metavar="num of dimensions for barplot "),
  make_option(c("-b", "--b"), type="integer", default=5, 
              help="number of dimensions to use in barplot", metavar="num of dimensions to use for barplot "),
  make_option(c("-c", "--clusters"), type="integer", default=3, 
              help="number of clusters", metavar="num of clusters "),
  make_option(c("-g", "--groups"), type="integer", default=15, 
              help="number of groups", metavar="num of groups "),
  make_option(c("-o", "--out"), type="character", default="out.txt", 
              help="output file name [default= %default]", metavar="output_file"),
  make_option(c("-r", "--dir"), type="character", default="~/", 
              help="directory to put the output", metavar="directory_name")
); 
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$file)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
}
dirname = opt$dir
dirname

# Import demo data
myGD=read.csv(file=opt$file)
myGD[1:10,1:10]
str(myGD)

# Get SVD
SVD <- svd(myGD[,-1])
str(SVD)

# Plot % contribution
POR <- SVD$d/sum(SVD$d)
pdf(file=(paste(dirname,"barplot1.pdf",sep="/")))
barplot(POR)
dev.off()
POR[which(POR>0.001)]*100

# Select number of dimensions to plot
#d <- 10 # parameters
d <- opt$a # what to label this in the UI?
pdf(paste(dirname,"barplot2.pdf",sep="/"))
barplot(POR[1:d])
dev.off()
POR[1:d]*100

# Define number of Dimensions to use
#k <- 5 # another parameter
k <- opt$b # what to label this in the UI?

# Create new data set with SVD scores and loadings
X <- data.frame(SVD$u[,1:k])
Y <- data.frame(SVD$v[,1:k])
colnames(X) <- colnames(Y) <- paste("D",seq(1,k),sep="")
X$Class <- c("Gen")
Y$Class <- c("Mrk")
X$Name <- myGD[,1]
Y$Name <- colnames(myGD[,-1])
Z <- rbind(X,Y)

write.table(x = Z, file = opt$out,
            quote = FALSE, # character without quotes
            sep = '\t', # <TAB> separeted file
            eol = '\n', # end of line as \n
            dec = '.', # decimal mark
            na = '.', # NA will be '.'
            row.names = FALSE, # without row names
            col.names = TRUE) # with collumn names

# BIPLOT
p <- ggplot(Z, aes(D1,D2, color=Class)) +
  geom_point(mapping = ggplot2::aes(name = Name))
# htmlwidgets::saveWidget(as.widget(p), "p_plot.html") # gives warnings: deprecated
ggplotly(p)
ggsave(paste(dirname,"p.jpg",sep="/"))

pz <- ggplot(X, aes(D2,D1)) +
  geom_point(mapping = ggplot2::aes(names=Name))
ggplotly(pz)
ggsave(paste(dirname,"pz.jpg",sep="/"))

py <- ggplot(Y, aes(D2,D1)) +
  geom_point(mapping = ggplot2::aes(names=Name))
ggplotly(py)
ggsave(paste(dirname,"py.jpg",sep="/"))

# Clustering

# Define number of dimensions
k <- opt$c # define a more comprehensive label: what are these dimensions?

# Define maxmimum number of groups
gg <- opt$g # make input a range of number of groups

MCX <- Mclust(X[,1:k], G=1:gg)

summary(MCX)

pdf(paste(dirname,"BIC.pdf",sep="/"))
plot(MCX,  what = c("BIC"))
dev.off()
pdf(paste(dirname,"classif.pdf",sep="/"))
plot(MCX,  what = c("classification"))
dev.off()
pdf(paste(dirname,"density.pdf",sep="/"))
plot(MCX,  what = c("density"))
dev.off()
pdf(paste(dirname,"uncertainty.pdf",sep="/"))
plot(MCX,  what = c("uncertainty"))
dev.off()
