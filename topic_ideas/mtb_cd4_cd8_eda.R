# Exploration of Mtb data

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
data <- read_excel("Lloyd21_Manuscript.xlsx", sheet = "CD4 and CD8 Counts")

# Choose one month(0) and one population(CD4)
data_cd4 <- data |>
  filter(MonthofSample == 0 & Population == "CD4")

data_cd8 <- data |>
  filter(MonthofSample == 0 & Population == "CD8")

################################################################################
# EDA
################################################################################

################################################################################
# Clustering
################################################################################
dismat_cd4 <- dist(data_cd4[,7:38], 
               method = "euclidean", 
               diag = TRUE, 
               upper = TRUE)

coords_cd4 <- metaMDS(dismat_cd4, k = 2) |> scores()
plot(coords_cd4)

# single seems most appropriate considering the elongated shape
hier_clust_cd4 <- dismat_cd4 |> 
  agnes(diss = TRUE, method = "single")

# Plot
pltree(hier_clust_cd4, main = "Dendrogram") 

# Assuming obs 43 has been removed
num_clusts_cd4 = 2:(nrow(data_cd4)-1)
# Silhouette widths
out_cd4 <- sapply(num_clusts_cd4, 
              FUN = function(k){mean(silhouette(cutree(hier_clust_cd4, 
                                                       k = k), 
                                                dismat_cd4)[,3])})
sc_cd4 <- max(out_cd4) # Best silhouette width
best_k_cd4 <- num_clusts_cd4[which(out_cd4 == sc_cd4)] # Associated k

# Cut tree to have 2 clusters 
hier_cut_cd4 <- cutree(hier_clust_cd4, k = best_k_cd4)

# Plot in 2D
hier_clust_data_cd4 <- cbind(clust = as.factor(hier_cut_cd4), as.data.frame(coords_cd4))

ggplot(hier_clust_data_cd4, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(col = clust))

