library(dplyr)
library(tidyverse)
library(DataExplorer)
library(purrr)
library(cluster)    
library(factoextra)
library(ggplot2)
library(highcharter)

# Read the data file
data <- read.csv(file.choose(), header = T)
orig_data <- data

# Data exploration
introduce(data)
summary(data)

# Remove country column
data_num <- data %>% select(-c(country))

## Check missng values
plot_intro(data, title="Missing Values")

## Child Mortality Distribution
hist(data_num$child_mort, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Child Mortality",
     main = "Child Mortality Distribution")
lines(density(data_num$child_mort), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

## Exports Distribution
hist(data_num$exports, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Exports",
     main = "Exports Distribution")
lines(density(data_num$exports), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

## Health Distribution
hist(data_num$health, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Health",
     main = "Health Distribution")
lines(density(data_num$health), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

## Imports Distribution
hist(data_num$imports, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Imports",
     main = "Imports Distribution")
lines(density(data_num$imports), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

## Life Expectancy Distribution
hist(data_num$life_expec, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Life Expectancy",
     main = "Life Expectancy")
lines(density(data_num$life_expec), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

## Total Fertility Distribution
hist(data_num$total_fer, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Total Fertility",
     main = "Fertility Distribution")
lines(density(data_num$total_fer), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

## GDPP Distribution
hist(data_num$gdpp, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "GDP",
     main = "GDP Distribution")
lines(density(data_num$gdpp), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

## Income Distribution
hist(data_num$income, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Income",
     main = "Income Distribution")
lines(density(data_num$income), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

## World map of child mortality of various nations
hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, orig_data, value = "child_mort", joinBy = c('name','country'),
    name = "Child Mortality of Various Nations"
  )  %>% 
  hc_colorAxis(minColor = "#ff7b7b", maxColor = "#a70000") %>% 
  hc_title(text = "Child Mortality of Various Nations")
hc

## World map of gdpp of various nations
hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, orig_data, value = "gdpp", joinBy = c('name','country'),
    name = "GDP Per Capita of Various Nations"
  )  %>% 
  hc_colorAxis(minColor = "#66b2b2", maxColor = "#006666") %>% 
  hc_title(text = "GDP Per Capita of Various Nations")
hc

## World map of Total Fertility of various nations
hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, orig_data, value = "total_fer", joinBy = c('name','country'),
    name = "Total Fertility of Various Nations"
  )  %>% 
  hc_colorAxis(minColor = "#980F5A", maxColor = "#4C0027") %>% 
  hc_title(text = "Total Fertility of Various Nations")
hc

## Income Level Distribution of various nations
hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, orig_data, value = "income", joinBy = c('name','country'),
    name = "Income Level Distribution of various nations"
  )  %>% 
  hc_colorAxis(minColor = "#E5EFC1", maxColor = "#557B83") %>% 
  hc_title(text = "Income Level Distribution of various nations")
hc

# Scale the data
data_scaled <- scale(data_num)

# Make country names into row names/id
rownames(data) <- data[,1]
data[,1] <- NULL
data_scaled <- scale(data)

################################################################################

# PCA 

# Run PCA, which performs singular-value decomposition
pc <- prcomp(data_scaled)
pc$rotation <- pc$rotation * -1 # Reflect loadings matrix for positive values
pc

# View variances
pc$sdev^2

# Plot the biplot
biplot(pc, cex=0.6, xlab="First Principal Component", 
       ylab="Second Principal Component", 
       panel.first = c(abline(h = 0, v = 0, col="lightgray", lty="dotted")))

# Scree plots
layout(matrix(1:2, ncol=2)) # Place next to each other
screeplot(pc, main="", col.lab="white") # Plot as bars
title(xlab="Principal Components", ylab="Variance")
axis(1, at=c(0,0.7,1.9,3.1,4.3), labels=c(0,1,2,3,4,5,6,7,8,9), pos=c(0,0))

screeplot(pc, type="lines", main="", col.lab="white") # Plot as lines
title(xlab="Principal Components", ylab="Variance")

# Calculate PVE
pve <- (pc$sdev^2) / sum(pc$sdev^2)
pve

# Plot PVE
plot(pve, type="b", pch=19, xlab="Principal Components", 
     ylab="Proportion of Variance Explained (PVE)", xaxt="n", ylim=c(0,1))
axis(1, at=c(1,2,3,4), labels=c(1,2,3,4), pos=c(-0.05,1))


cum_pve <- numeric(length(pve))
cum_pve[1] <- pve[1]
for (i in 2:length(cum_pve)){
  cum_pve[i] <- pve[i] + cum_pve[i-1]
}
cum_pve

plot(cum_pve, type="b", pch=19, xlab="Combined Principal Components", ylab="Cumulative PVE", xaxt="n", ylim=c(0,1))
axis(1, at=c(1,2,3,4), labels=c(1,2,3,4), pos=c(-0.05,1))

# Looks like there are 4 PC's that we need to take into account
# Next job is to find the PC scores for each country and then create the new data table cluster

# PC1 puts most weight on child mortality and total_fertility (Birth Rate)
# PC2 puts most weight on exports and imports (Trade)
# PC3 puts most weight on health and inflation (inflation)
# PC4 puts most weights on income and gdpp (Economy)
 

################################################################################

# Hierarchical clustering


################################################################################
# K-Means Clustering

# Elbow Method
set.seed(490)
fv_nbclust(data_scaled,kmeans,method = 'wss')

# Silhouette Method
fviz_nbclust(data_scaled, kmeans, method = "silhouette")

#compute gap statistic
gap_stat <- clusGap(data_scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

# K means using 4 clusters
final <- kmeans(data_scaled, 4, nstart = 25)
print(final)
fviz_cluster(final, data = data_scaled)

data_final <- data_scaled
rownames(data_final) <- orig_data$country
final$cluster
cluster_data <- as.data.frame(final$cluster)
head(cluster_data)

# K Means using 3 clusters
final <- kmeans(data_final, 3, nstart = 25)
print(final)
fviz_cluster(final, data = data_final)
cluster_data <- as.data.frame(final$cluster)
head(cluster_data)

total_data <- cbind(orig_data,cluster = final$cluster)
head(total_data)
colnames(total_data)[11] <- "cluster"
colnames(total_data)

aggregate(total_data[, 2:10], list(total_data$cluster), mean)
str(total_data)

# Multiple conditions when adding new column to dataframe:
total_data <- total_data %>% mutate(group =
                                    case_when(cluster == 1 ~ 2, 
                                              cluster == 2 ~ 1,
                                              cluster == 3 ~ 3)
)
# Plot Map
total_data$cluster <- as.numeric(total_data$cluster)
hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, total_data, value = "group", joinBy = c('name','country'),
    name = "Help"
  )  %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_title(text = "World Map") %>% 
  hc_subtitle(text = "Yellow = No Risk, Blue = High Risk, Green = Medium Risk ") 
hc

# Kmeans using PCA
new_data <- pc[["x"]]
new_data <- new_data[,-c(6,7,8,9)]
new_data <- new_data*100

# Elbow Method
set.seed(490)
fviz_nbclust(new_data,kmeans,method = 'wss')

# Silhouette Method
fviz_nbclust(new_data, kmeans, method = "silhouette")

#compute gap statistic
gap_stat <- clusGap(new_data, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

final <- kmeans(new_data, 3, nstart = 25)
print(final)
fviz_cluster(final, data = new_data)

pc_data_kmeans <- cbind(orig_data,cluster = final$cluster)
head(pc_data_kmeans)
colnames(pc_data_kmeans)[11] <- "cluster"
colnames(pc_data_kmeans)

aggregate(pc_data_kmeans[, 2:10], list(pc_data_kmeans$cluster), mean)
str(pc_data_kmeans)

# Multiple conditions when adding new column to dataframe:
pc_data_kmeans <- pc_data_kmeans %>% mutate(group =
                                      case_when(cluster == 1 ~ 3, 
                                                cluster == 2 ~ 2,
                                                cluster == 3 ~ 1)
)

# Plot Map
pc_data_kmeans$cluster <- as.numeric(pc_data_kmeans$cluster)
hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, pc_data_kmeans, value = "group", joinBy = c('name','country'),
    name = "Help"
  )  %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_title(text = "World Map (PCA K-Means)") %>% 
  hc_subtitle(text = "Yellow = No Risk, Blue = High Risk, Green = Medium Risk ")
hc
