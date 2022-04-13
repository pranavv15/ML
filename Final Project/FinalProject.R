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

# Remove country column
data_num <- data %>% select(-c(country))

# Scale the data
data_scaled <- scale(data_num)

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









