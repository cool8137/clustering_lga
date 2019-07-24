# --------------
# Name: cluster_analysis.R
# Author: Prashidha Kharel
# Description: This script performs PCA analysis for dimentionality reduction on the 
# CSV data output by data_rep.R. Then using the selected eigen vectors, it will perform
# cluster analysis.

library(tidyverse)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# Data Prep ------------

# Read cleaned LGA Stat data
lga_df <- read_csv("./CSV/LGA_STAT_TABLE_CLEAN.csv")

# isolate just the values and convert it to matrix
lga_mat <- as.matrix(lga_df[, 2:ncol(lga_df)])
rownames(lga_mat) <- lga_df$`LGA Name`

# replace NA with 0 (because percentage is very low so they didn't appear)
lga_mat[is.na(lga_mat)] <- 0

# Dimensionality Reducation ----------------------

# Perform PCA
lga.pca <- prcomp(lga_mat, center = TRUE, scale. = TRUE)

plot(lga.pca, type = "l")

lga.pca.summary <- summary(lga.pca)

# dimensions to select
no_dim <- 5

lga_mat_reduced <- lga.pca$x[, 1:no_dim]

# K means clustering -------------------------

fviz_nbclust(lga_mat_reduced, kmeans, method = "wss")

fviz_nbclust(lga_mat_reduced, kmeans, method = "silhouette")

gap_stat <- clusGap(lga_mat_reduced, FUN = kmeans, nstart = 25,
                    K.max = 1000, B = 50)
fviz_gap_stat(gap_stat)

no_clusters <- 5

lga_kmeans <- kmeans(lga_mat_reduced, no_clusters, iter.max = 100000)

lga_km_eigen_centers <- lga_kmeans$centers

scale_mat <- matrix(rep(lga.pca$scale, no_clusters), byrow = TRUE, nrow = no_clusters)
centre_mat <- matrix(rep(lga.pca$center, no_clusters), byrow = TRUE, nrow = no_clusters)
lga_km_centers <- (lga_km_eigen_centers %*% t(lga.pca$rotation[,1:no_dim])) * scale_mat  + centre_mat

ggplot(lga_kmeans, data = lga_mat_reduced)



