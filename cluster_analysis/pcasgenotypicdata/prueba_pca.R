#########################################################
#                                                       #
# April  17, 2018                                       #
# Singular Value Decomposition of Genetic matrix        #
# Cluster analysis                                      #
#                                                       #
#########################################################

list.of.packages <- c("rpca")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(plotly)
library(mclust)

rm(list = ls())

# Import demo data
myGD <- read.csv(file = "Mrk_numeric.csv")

myGD[1:10,1:10]


str(myGD)

# Get SVD
SVD <- svd(myGD[,-1])
str(SVD)

# Plot % contribution
POR <- SVD$d/sum(SVD$d)

barplot(POR)
POR[which(POR>0.001)]*100

# Select number of dimensions to plot
d <- 10 # parameters
barplot(POR[1:d])
POR[1:d]*100

# Define number of Dimensions to use
k <- 5 # another parameter

# Create new data set with SVD scores and loadings
X <- data.frame(SVD$u[,1:k])
Y <- data.frame(SVD$v[,1:k])
colnames(X) <- colnames(Y) <- paste("D",seq(1,k),sep="")
X$Class <- c("Gen")
Y$Class <- c("Mrk")
X$Name <- myGD[,1]
Y$Name <- colnames(myGD[,-1])
Z <- rbind(X,Y)

write.table(x = Z, file = 'output.txt',
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

p

px <- ggplot(X, aes(D2,D1)) +
     geom_point(mapping = ggplot2::aes(names=Name))

px

pz <- ggplot(Y, aes(D2,D1)) +
     geom_point(mapping = ggplot2::aes(names=Name))

pz

## interactive plots!
out <- ggplotly(p)
ggplotly(px)
ggplotly(pz)

str(out)

# Clustering

# Define number of dimensions
k <- 3 # parameter too
# Define maxmimum number of groups
gg <- 15 # maximum number of groups

MCX <- Mclust(X[,1:k], G=1:gg)


summary(MCX)

plot(MCX,  what = c("BIC"))
plot(MCX,  what = c("classification"))
plot(MCX,  what = c("density"))
plot(MCX,  what = c("uncertainty"))

