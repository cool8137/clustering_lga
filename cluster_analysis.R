# --------------
# Name: cluster_analysis.R
# Author: Prashidha Kharel
# Description: This script performs PCA analysis for dimentionality reduction on the 
# CSV data output by data_rep.R. Then using the selected eigen vectors, it will perform
# cluster analysis.

library(tidyverse)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(rgdal)

# Data Prep ------------

# Read cleaned LGA Stat data
lga_df <- read_csv("./CSV/LGA_STAT_TABLE_CLEAN.csv")

# isolate just the values and convert it to matrix
lga_mat <- as.matrix(lga_df[, -c(1,ncol(lga_df))])
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

ggplot(as.data.frame(lga_mat_reduced), aes(PC1, PC2, color = factor(lga_kmeans$cluster))) +
  geom_point()

ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$cluster)) + geom_point()

# Cluster on MAP ----------------------

# load the LGA shp data
# Read all shape files in the SHP_FILES directory
shp_dir <- "./SHP_FILES/"
shp_names <- dir(shp_dir)
shp_list <- list()
for (shp_name in shp_names) {
  shp_path <- file.path(shp_dir, shp_name)
  shp_list[[shp_name]] <- readOGR(shp_path, shp_name)
}

shp_state_ab <- sapply(shp_names, function(x) strsplit(x,"_")[[1]][1])

# Add cluster info to each of the shape files
for (shp_name in shp_names) {
  shp_list[[shp_name]]@data[[8]]
}
