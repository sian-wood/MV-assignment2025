# Exploration of US votes per county data

################################################################################
# Install packages
################################################################################
library(readxl)
library(dplyr)
library(cluster)
library(vegan)
library(ggplot2)
library(mclust)

################################################################################
# Read in data
################################################################################
# Create dataframe
data <- readLines("space_ga.txt")[-(1:32)] |> as.data.frame()
mat <- matrix(NA, nrow = nrow(data), ncol = 7)
colnames(mat) <- c("n_votes", "pop", "edu", "houses", "income", "xcoord", "ycoord")

# Convert df rows from single strings to vectors of numerics
for(i in 1:nrow(data)){
  # Split lines into vectors of elements
  row <- strsplit(data[i,], " ")[[1]]         
  # Remove empty elements and convert from strings to numerics
  row <- row[row != ""] |> as.numeric()  
  mat[i,] <- row
}


################################################################################
# EDA
################################################################################
nrow(mat) #3106
colnames(mat)

hist(mat[,1])
hist(mat[,2])
hist(mat[,3])
hist(mat[,4])
hist(mat[,5])
hist(mat[,6])
hist(mat[,7])

ranges <- as.data.frame(matrix(NA, nrow = ncol(data_us[7:38]), ncol = 2))
row.names(ranges) <- colnames(data_us[7:38])


for(col in 1:ncol(data_us[7:38])){
  ranges[col,] <- range(data_us[7:38][,col], na.rm = TRUE)
}

################################################################################
# Clustering
################################################################################
# Remove response variable, longitude and latitude
dismat <- dist(mat[,2:5], 
               method = "euclidean", 
               diag = TRUE, 
               upper = TRUE)

coords <- metaMDS(dismat, k = 2) |> scores()
plot(coords)


hier_clust <- dismat |> 
  agnes(diss = TRUE, method = "average")

# Plot
pltree(hier_clust, main = "Dendrogram") 

# Assuming obs 43 has been removed
num_clusts = 2:(nrow(mat)-1)
# Silhouette widths
out <- sapply(num_clusts, 
              FUN = function(k){mean(silhouette(cutree(hier_clust, 
                                                       k = k), 
                                                dismat)[,3])})
sc <- max(out) # Best silhouette width
best_k <- num_clusts[which(out == sc)] # Associated k

# Cut tree to have 2 clusters 
hier_cut <- cutree(hier_clust, k = best_k)

# Plot in 2D
hier_clust_data <- cbind(clust = as.factor(hier_cut), as.data.frame(coords))

ggplot(hier_clust_data, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(col = clust))


