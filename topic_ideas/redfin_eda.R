# Exploration of Redfin data

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
data <- read_excel("ZivaHub_RawData.xlsx", sheet = "Raw Data")
# Neaten dataframe
colnames(data) <- data[1,]
data <- data[2:nrow(data),]
data <- data |> mutate(across(everything(), as.numeric))
# Update column names so easier to work with 
colnames(data)[c(10,13)] <- c("Total_Iron", "Inorg_Nitrogen")
# Remove response var, factor/binary vars, and other species found (feels like some species may always be found together)
data <- data[,1:16]
colnames(data)
# Remove factor variables

################################################################################
# EDA
################################################################################

################################################################################
# Clustering
################################################################################
dismat <- dist(data[,2:14], 
               method = "euclidean", 
               diag = TRUE, 
               upper = TRUE)

coords <- metaMDS(dismat, k = 2) |> scores()
plot(coords)

# Try without obs 43

dismat <- dist(data[-43,2:14], 
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
num_clusts = 2:(nrow(data[-43,])-1)
# Silhouette widths
out <- sapply(num_clusts, 
              FUN = function(k){mean(silhouette(cutree(hier_clust, 
                                                       k = k), 
                                                dismat)[,3])})
sc <- max(out) # Best silhouette width
best_k <- num_clusts[which(out == sc)] # Associated k

# Cut tree to have 2 clusters 
hier_cut2 <- cutree(hier_clust, k = 2)

# Plot in 2D
hier_clust_data <- cbind(clust = as.factor(hier_cut2), as.data.frame(coords))

ggplot(hier_clust_data, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(col = clust))


