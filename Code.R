
## Installing and loading libraries required for EDA 
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)
library(dtplyr)

## Output of each cluster
outputs<-list()

################################### INITIAL EXPLORATORY ANALYSIS ####################################

## Importing and Viewing
data <- Oil.and.Gas.1932.2014
data <- data[,6:ncol(data)]
View(data)
print(data)

## Check the class 
class(data)

## Check the number of rows and columns
dim(data)

## Structure of the data
str(data)
numeric_var <- names(data)[which(sapply(data, is.numeric))]
cat_var <- names(data)[which(sapply(data, is.character))]


## View summary statistics
summary(data)

## Identifying missing values
head(data)
colSums(sapply(data, is.na))
library(data.table)
setDT(data)
summary(data[,.SD, .SDcols =numeric_var])
colSums(sapply(data[,.SD, .SDcols = numeric_var], is.na))
summary(data[,.SD, .SDcols =cat_var])
colSums(sapply(data[,.SD, .SDcols = cat_var], is.na))

## The percentage of data missing.
sum(is.na(data)) / (nrow(data) *ncol(data))

# Check for duplicated rows.
cat("The number of duplicated rows are", nrow(data) - nrow(unique(data)))

################################### VISUAL EXPLORATORY ANALYSIS #####################################

## Visualization of the missing values
setDF(data)

plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}
plot_Missing(data[,colSums(is.na(data)) > 0])

## Density plots for numeric variables.
setDT(data)
data_cont <- data[,.SD,.SDcols = numeric_var]

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}
plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$net_oil_gas_exports_valuePOP)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}

View(data)
doPlots(data_cont, fun = plotDen, ii = 1:4, ncol = 2)
doPlots(data_cont, fun = plotDen, ii = 5:10, ncol = 2)
doPlots(data_cont, fun = plotDen, ii = 11:16, ncol = 2)
doPlots(data_cont, fun = plotDen, ii = 17:24, ncol = 2)
doPlots(data_cont, fun = plotDen, ii = 25:30, ncol = 2)
doPlots(data_cont, fun = plotDen, ii = 32:36, ncol = 2)

## Explore correlation
df<-data
colnames(df)<-c()
setDF(df)
df[is.na(df)] <- 0
corrplot(cor(df), method="pie", type="upper",is.corr=FALSE)

## Plot scatter plots
setDT(data)

library(scales)
ggplot(data, aes(x=mult_nom_2000)) + geom_histogram(col = 'white') + theme_light() +scale_x_continuous(labels = comma)
## Normalize distribution
ggplot(data, aes(x=log(mult_nom_2000+1))) + geom_histogram(col = 'white') + theme_light()

## Visualizing using a box plot
ggplot(data = data, mapping = aes(x = mult_nom_2000, y = mult_2000_2014)) + 
  geom_boxplot(mapping = aes(group = cut_number(mult_nom_2000, 20)))

###################################### DATA PRE-PROCESSING ##########################################

setDF(data)

## DATA CLEANING
## Missing data [Conversion of NA to 0]
data[is.na(data)] <- 0

## DATA TRANSFORMATION
## Min Max Normalization
normalise<-function(X){
  return((X-min(X))/(max(X)-min(X)))
}

for(i in 1:ncol(data)){
  if(is.numeric(data[,i]))
    data[,i]<-normalise(data[,i])
}
print(data)

## DATA REDUCTION
## Remove rows with same value throughout
data <- data[,-ncol(data)]

## Feature Selection
## Correlation (removing redundant features)

## Subsetting the data and selecting only required variables
df <- data
dim(df)
for(i in 1:ncol(df)){
  if(!is.numeric(df[,i])){
    df <- df[,-i]
  }
}
dim(df)

# Using corr function to generate correlation matrix
print(cor(df))

# Building correplot to visualize the correlartion matrix
library(corrplot)
colnames(df)<-c()
corrplot(cor(df), method="number", is.corr=FALSE)

library(caret)
highlyCorrelated <- findCorrelation(cor(df), cutoff=0.75)
library(sets) 
lessCorrelated<-setdiff(c(1:ncol(df)),highlyCorrelated)
print(lessCorrelated)
print(length(lessCorrelated))

df <- df[, lessCorrelated]
data <- df
print(data)

############################################## PCA ##################################################

dataPCA <- data

## Compute PCA
dataPCA <- prcomp(dataPCA, center= TRUE)
summary(dataPCA)

## Plot the first two PCs
plot(dataPCA$x[,1],dataPCA$x[,2])
## Plot the first three PCs
scatterplot3d::scatterplot3d(x=dataPCA$rotation[,'PC1'], y=dataPCA$rotation[,'PC2'], z=dataPCA$rotation[,'PC3'])

## Visualizing the variance
dataPCA.var<-dataPCA$sdev^2
dataPCA.var.per<-round(dataPCA.var/sum(dataPCA.var)*100,1)
barplot(dataPCA.var.per,main="Scree Plot",xlab="Principal Component",
        ylab="Percentage Variation")

## Visualize eigenvalues. Show the percentage of variances explained by each principal component.
library("factoextra")
fviz_eig(dataPCA)

biplot(dataPCA)
library(ggfortify)
autoplot(dataPCA, loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE,ellipse=TRUE)

########################################### CLUSTERING ##############################################
############################################# DBSCAN ################################################

library(dbscan)
dataDBSCAN <- as.matrix(data)

## Find suitable eps parameter using a k-NN plot for k = dim + 1
## Look for the knee!
kNNdistplot(dataDBSCAN, k = ncol(data)+1)
abline(h=.2, col = "red", lty=2)
res <- dbscan::dbscan(dataDBSCAN, eps = .2, MinPts = ncol(dataDBSCAN)+1)
print(res)
pairs(dataDBSCAN, col = res$cluster + 1L)

## Cluster Visualization
library("factoextra")
fviz_cluster(res,dataDBSCAN,geom="point")

hullplot(dataDBSCAN, res)
outputs[['DBSCAN']]<-res

## Use precomputed frNN
fr <- frNN(dataDBSCAN, eps = .2)
dbscan(fr, MinPts = ncol(dataDBSCAN)+1)

## Use data from fpc
library("fpc")
res <- fpc::dbscan(dataDBSCAN, eps = .2, MinPts = ncol(dataDBSCAN)+1)
res
## Plot clusters and add noise (cluster 0) as crosses.
plot(dataDBSCAN, col=res$cluster)
points(dataDBSCAN[res$cluster==0,], pch = 3, col = "grey")

hullplot(dataDBSCAN, res)

## Predict cluster membership for new data points
## (Note: 0 means it is predicted as noise)
newdata <- dataDBSCAN[10:50,] 
predict(res, newdata, data = dataDBSCAN)

## Compare speed against fpc version (if microbenchmark is installed)
## We use dbscan::dbscan to make sure that we do now run the implementation in fpc.

if (requireNamespace("fpc", quietly = TRUE) &&
    requireNamespace("microbenchmark", quietly = TRUE)) {
  t_dbscan <- microbenchmark::microbenchmark(
    dbscan::dbscan(dataDBSCAN, .3, 3), times = 10, unit = "ms")
  t_dbscan_linear <- microbenchmark::microbenchmark(
    dbscan::dbscan(dataDBSCAN, .3, 3, search = "linear"), times = 10, unit = "ms")
  t_dbscan_dist <- microbenchmark::microbenchmark(
    dbscan::dbscan(dataDBSCAN, .3, 3, search = "dist"), times = 10, unit = "ms")
  t_fpc <- microbenchmark::microbenchmark(
    fpc::dbscan(dataDBSCAN, .3, 3), times = 10, unit = "ms")
  r <- rbind(t_fpc, t_dbscan_dist, t_dbscan_linear, t_dbscan)
  r
  boxplot(r,
          names = c('fpc', 'dbscan (dist)', 'dbscan (linear)', 'dbscan (kdtree)'),
          main = "Runtime comparison in ms")
  ## Speedup of the kd-tree-based version compared to the fpc implementation
  median(t_fpc$time) / median(t_dbscan$time)
}

############################################# K-MEANS ###############################################

dataKMEANS <- data

k2 <- kmeans(dataKMEANS, centers = 2, nstart = 25)
str(k2)
k2
library("factoextra")
fviz_cluster(k2, data = dataKMEANS)
hullplot(dataKMEANS, k2)
k3 <- kmeans(dataKMEANS, centers = 3, nstart = 25)
k4 <- kmeans(dataKMEANS, centers = 4, nstart = 25)
k5 <- kmeans(dataKMEANS, centers = 5, nstart = 25)

## Plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = dataKMEANS) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = dataKMEANS) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = dataKMEANS) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = dataKMEANS) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

## Silhouettes to find optimal cluster size
library("cluster")
avg_sil <- function(k) {
  km.res <- kmeans(dataKMEANS, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(dataKMEANS))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# Extract avg silhouette for 2-15 clusters
library("purrr")
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

## Alternate to the above
fviz_nbclust(dataKMEANS, kmeans, method = "silhouette") 

## Storing the output
outputs[['KMEANS']]<-k2

############################################# OPTICS ################################################

dataOPTICS <- data

## Run OPTICS 
res <- optics(dataOPTICS, eps=0.5, minPts = 10)
res

## Get order
res$order

## Plot produces a reachability plot
plot(res)

## Plot the order of points in the reachability plot
plot(dataOPTICS, col = "grey")
polygon(dataOPTICS[res$order,])

## Extract a DBSCAN clustering by cutting the reachability plot at eps_cl
res <- extractDBSCAN(res, eps_cl = .065)
res
plot(res) ## black is noise
hullplot(dataOPTICS, res)

## Re-cut at a higher eps threshold
res <- extractDBSCAN(res, eps_cl = .2)
res
plot(res)
hullplot(dataOPTICS, res)
outputs[['OPTICS']]<-res

## Extract hierarchical clustering of varying density using the Xi method
res <- extractXi(res, xi = 0.05)
res
plot(res)
hullplot(dataOPTICS, res)

## Xi cluster structure
res$clusters_xi

## Use OPTICS on a precomputed distance matrix
d <- dist(dataOPTICS)
res <- optics(d, minPts = 10)
plot(res) 

######################################### POST-ANALYSIS ############################################

par(mfrow=c(1,1))
hullplot(data,outputs[['DBSCAN']],main="DBSCAN")
hullplot(data,outputs[['KMEANS']],main="K-MEANS")
hullplot(data,outputs[['OPTICS']],main="OPTICS")



