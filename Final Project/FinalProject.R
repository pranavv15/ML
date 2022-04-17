library(dplyr)
library(tidyverse)
library(DataExplorer)
library(purrr)
library(cluster)    # clustering algorithms
library(factoextra)

# Read the data file
data <- read.csv(file.choose(), header = T)

# Data exploration
introduce(data)

summary(data)

# Make country names into row names/id
rownames(data) <- data[,1]
data[,1] <- NULL

data_scaled <- scale(data)

#################################################################################################

# PCA 

# Run PCA, which performs singular-value decomposition
pc <- prcomp(data_scaled)
pc$rotation <- pc$rotation * -1 # Reflect loadings matrix for positive values
pc

# View variances
pc$sdev^2

# Plot the biplot
biplot(pc, cex=0.6, xlab="First Principal Component", ylab="Second Principal Component", panel.first = c(abline(h = 0, v = 0, col="lightgray", lty="dotted")))

# Scree plots
layout(matrix(1:2, ncol=2)) # Place next to each other
screeplot(pc, main="", col.lab="white") # Plot as bars
title(xlab="Principal Components", ylab="Variance")
axis(1, at=c(0,0.7,1.9,3.1,4.3), labels=c(0,1,2,3,4), pos=c(0,0))

screeplot(pc, type="lines", main="", col.lab="white") # Plot as lines
title(xlab="Principal Components", ylab="Variance")

# Calculate PVE
pve <- (pc$sdev^2) / sum(pc$sdev^2)
pve

# Plot PVE
plot(pve, type="b", pch=19, xlab="Principal Components", ylab="Proportion of Variance Explained (PVE)", xaxt="n", ylim=c(0,1))
axis(1, at=c(1,2,3,4), labels=c(1,2,3,4), pos=c(-0.05,1))


cum_pve <- numeric(length(pve))
cum_pve[1] <- pve[1]l
for (i in 2:length(cum_pve)){
  cum_pve[i] <- pve[i] + cum_pve[i-1]
}
cum_pve

plot(cum_pve, type="b", pch=19, xlab="Combined Principal Components", ylab="Cumulative PVE", xaxt="n", ylim=c(0,1))
axis(1, at=c(1,2,3,4), labels=c(1,2,3,4), pos=c(-0.05,1))

# Looks like there are 4 PC's that we need to take into account
# Next job is to find the PC scores for each country and then create the new data table cluster

# To the best of my guess PC1 is life expectancy,gdpp and income / might also include child mortality and total_fertility
# PC2 puts most weight on exports and imports
# PC3 puts most weights on inflation
# PC4 puts most weight on health

####################################################################################################

# Hierarchical clustering

# Using the new clusters are our new data
new_data <- pc[["x"]]
new_data <- new_data[,-c(5,6,7,8,9)]
new_data <- new_data*100

new_data <- as.data.frame(new_data)
new_data2 <- scale(new_data)


# define linkage methods to use
linkage <- c( "average", "single", "complete", "ward")
names(linkage) <- c( "average", "single", "complete", "ward")

# agnes: Compute agglomerative hierarchical clustering of the dataset
ac <- function(x) {
  agnes(new_data2, method = x)$ac
}
# calculate agglomerative coefficient for each clustering linkage method
sapply(linkage, ac)

# While all have similar scores, Ward performs the best

# Dendrogram
clust <- agnes(new_data2, method = "ward")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram")

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(new_data2, FUN = hcut, nstart = 25, K.max = 10, B = 50)
gap_stat

#produce plot of gap statistic vs. number of clusters 
fviz_gap_stat(
  gap_stat,
  linecolor = "red",
  maxSE = list(method = "firstSEmax", SE.factor = 0)
)

# Optimal number of clusters unclear because gap stat keeps on decreasing, recommends 4

#compute distance matrix
d <- dist(new_data2, method = "euclidean")

# perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

# cut the dendrogram into 4 clusters based on the Gap Statistic
clusters <- cutree(final_clust, k=4)

# find number of observations in each cluster
table(clusters)

# append cluster labels to original data
data3 <- new_data
final_data3 <- cbind(new_data, cluster = clusters)
final_data3 <- as.data.frame(final_data3)

# display first 20 rows of final data
head(final_data3, 20)

# find mean values for each cluster
aggregate(final_data3, by=list(cluster=final_data3$cluster), mean)

# Silhouette Analysis
cl <- hclust(as.dist(d,diag = TRUE, upper = TRUE), method= 'ward')
sil_cl <- silhouette(clusters ,as.dist(d), title=title(main = 'Good'))

rownames(sil_cl) <- rownames(d)

plot(sil_cl)

# Make country column
final_data3$Country <- rownames(final_data3)
final_data3['United States', "Country"] <- "United States of America"
final_data3 <- as.data.frame(final_data3)


# 
# ## Silhouette Analysis
# d <- dist(new_data2, method = "euclidean") # Calculate distance matrix
# sils <- list()
# avg.sil <- numeric(9)
# for(i in 1:9){
#   output <- silhouette(final_data3$cluster, d)
#   # average the scores
#   sils[[i]] <- output
#   avg.sil[i] <- mean(outßput[,3])
# }
# # Obtain the silhouette scores for 3 and 4 clusters
# sil3 <- sils[[2]]
# sil4 <- sils[[3]]
# 
# # Plot the scores for each observation
# plot(sil4, main="Silhouette Plot: Hierarchical, 4 Clusters", col=1:3, border=NA)


# Geospatial Plot of clusters
library(highcharter)

hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, final_data3, value = "cluster", joinBy = c('name','Country'),
    name = "Help"
  )  %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_title(text = "World Map") %>% 
  hc_subtitle(text = "Help Needed by Countries ")
hc

################################################################################################

# Hierarchical clustering with original data

# define linkage methods to use
linkage <- c( "average", "single", "complete", "ward")
names(linkage) <- c( "average", "single", "complete", "ward")

# agnes: Compute agglomerative hierarchical clustering of the dataset
ac <- function(x) {
  agnes(data_scaled, method = x)$ac
}
# calculate agglomerative coefficient for each clustering linkage method
sapply(linkage, ac)

# While all have similar scores, Ward performs the best

# Dendrogram
clust <- agnes(data_scaled, method = "ward")
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram")

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(data_scaled, FUN = hcut, nstart = 25, K.max = 10, B = 50)
gap_stat

#produce plot of gap statistic vs. number of clusters 
fviz_gap_stat(
  gap_stat,
  linecolor = "red",
  maxSE = list(method = "firstSEmax", SE.factor = 0)
)

# Optimal number of clusters unclear because gap stat keeps on increasing, recommends 3 

#compute distance matrix
d <- dist(data_scaled, method = "euclidean")

# perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

# cut the dendrogram into 4 clusters based on the Gap Statistic
clusters <- cutree(final_clust, k=3)

# find number of observations in each cluster
table(clusters)

# append cluster labels to original data
data2 <- data
final_data2 <- cbind(data2, cluster = clusters)
final_data <- as.data.frame(final_data2)

# find mean values for each cluster
aggregate(final_data2, by=list(cluster=final_data2$cluster), mean)

# Silhouette Analysis
cl <- hclust(as.dist(d,diag = TRUE, upper = TRUE), method= 'ward')
sil_cl <- silhouette(clusters,as.dist(d), title=title(main = 'Good'))

rownames(sil_cl) <- rownames(d)

plot(sil_cl)


# display first 20 rows of final data
head(final_data2, 20)

# Make country column
final_data2$Country <- rownames(final_data2)
final_data2['United States', "Country"] <- "United States of America"


# ## Silhouette Analysis
# d <- dist(data_scaled, method = "euclidean") # Calculate distance matrix
# sils <- list()
# avg.sil <- numeric(9)
# for(i in 1:9){
#   output <- silhouette(fin[[i]]$cluster, d)
#   # average the scores
#   sils[[i]] <- output
#   avg.sil[i] <- mean(output[,3])
# }
# # Obtain the silhouette scores for 3 and 4 clusters
# sil3 <- sils[[2]]
# sil4 <- sils[[3]]


# Geospatial Plot of clusters
library(highcharter)

hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, final_data2, value = "cluster", joinBy = c('name','Country'),
    name = "Help"
  )  %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_title(text = "World Map") %>% 
  hc_subtitle(text = "Help Needed by Countries ")
hc

par(mfrow = c(1,1))
summary(final_data2)
#################################################################################################

# References 

# https://stackoverflow.com/questions/30261435/r-clustering-silhouette-with-observation-labels
# https://www.datanovia.com/en/lessons/highchart-interactive-world-map-in-r/

