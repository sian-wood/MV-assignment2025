# Sian Wood
# 15/03/2025

################################################################################
# Load packages
################################################################################
library(readxl)
library(dplyr)
library(cluster)
library(vegan)
library(ggplot2)
library(mclust)
library(xtable)
library(corrplot)

################################################################################
# Read in data
################################################################################
data_rf <- read_excel("topic_ideas\\ZivaHub_RawData.xlsx", sheet = "Proportional Converted to %")

# Neaten dataframe
colnames(data_rf) <- data_rf[1,]
data_rf <- data_rf[2:nrow(data_rf),]
data_rf <- data_rf |> mutate(across(everything(), as.numeric))

# Update column names so easier to work with 
colnames(data_rf)[c(10,13)] <- c("Total_Iron", "Inorg_Nitrogen")

# Split into test and training 
set.seed(64)
train_rows = sample(1:nrow(data_rf), size = ceiling(0.75*nrow(data_rf)))
train = data_rf[train_rows,]
test = data_rf[-train_rows,]

# Ensure that at least one site with presence of each species occurs in each set
# Heuningnes Redfin
sum(train$`Heuningnes Redfin`>0)
sum(test$`Heuningnes Redfin`>0)
# Cape Kurper
sum(train$`Cape Kurper`>0)
sum(test$`Cape Kurper`>0)
# Cape Galaxias
sum(train$`Cape Galaxias`>0)
sum(test$`Cape Galaxias`>0)
# Spotted Bass
sum(train$`Spotted Bass`>0)
sum(test$`Spotted Bass`>0)
# Bluegill Sunfish
sum(train$`Bluegill Sunfish`>0)
sum(test$`Bluegill Sunfish`>0)
# Common Carp
sum(train$`Common Carp`>0)
sum(test$`Common Carp`>0)

# Only Common Carp must be removed
# Remove binary vars while I'm at it
cc_col = which(names(train) == "Common Carp")
train  = train[,-c(cc_col, cc_col+1, cc_col+2)]
test   = test[,-c(cc_col, cc_col+1, cc_col+2)]

################################################################################
# Correlations
################################################################################
corrplot(cor(train[,2:45]), method = "color", type = "upper", 
         diag = FALSE,  
         tl.col = "black", tl.srt = 35, addCoef.col = "black",
         tl.cex = 0.4, number.cex = 0.3)
#' Spotted bass may be well predicted by undercut riverbanks
#' Cape galaxias have a number of relatively strong pos and neg correlations, 
#' particularly with water quality vars

################################################################################
# Nonlinear MDS
################################################################################
dismat <- dist(train[,2:40], 
               method = "euclidean", 
               diag = TRUE, 
               upper = TRUE)

coords <- metaMDS(dismat, k = 2) |> scores()
coords <- cbind(Sites = train$Sites, as.data.frame(coords))
ggplot(coords, aes(x = NMDS1, y = NMDS2, label=Sites)) +
  geom_point() + 
  geom_text(hjust = -0.25)

hier_clust <- dismat |> 
  agnes(diss = TRUE, method = "average")

# Plot
pltree(hier_clust, main = "Dendrogram") 

# Assuming obs 43 has been removed
num_clusts = 2:(nrow(train)-1)
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

ggplot(hier_clust_data, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
  geom_point(aes(col = clust)) + 
  geom_text(hjust = -0.25,aes(col = clust))

# colour by presence of fish at a site
fish_binary = apply(train[,41:45], MARGIN = c(1, 2), FUN = function(x){if(x>0) 
  return(as.factor(1)) 
  else{return(as.factor(0))}})
coords_fish = cbind(coords, fish_binary)

# Heuningnes Redfin
ggplot(coords_fish, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
  geom_point(aes(col = `Heuningnes Redfin`)) + 
  geom_text(hjust = -0.25, aes(col = `Heuningnes Redfin`))

# Cape Kurper
ggplot(coords_fish, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
  geom_point(aes(col = `Cape Kurper`)) + 
  geom_text(hjust = -0.25, aes(col = `Cape Kurper`))

# Cape Galaxias
ggplot(coords_fish, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
  geom_point(aes(col = `Cape Galaxias`)) + 
  geom_text(hjust = -0.25, aes(col = `Cape Galaxias`))
# cluster well

# Spotted Bass
ggplot(coords_fish, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
  geom_point(aes(col = `Spotted Bass`)) + 
  geom_text(hjust = -0.25, aes(col = `Spotted Bass`))
# low NMDS2, high NMDS1

# Bluegill Sunfish
ggplot(coords_fish, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
  geom_point(aes(col = `Bluegill Sunfish`)) + 
  geom_text(hjust = -0.25, aes(col = `Bluegill Sunfish`))
# low NMDS2, high NMDS1

################################################################################
# Linear PCA
################################################################################
pca = prcomp(train[2:40], retx = TRUE, center = TRUE, scale = TRUE)

sum(pca$sdev^2>1)
# Choose 13
sum((pca$sdev^2)[1:13])/sum(pca$sdev^2) # 86%

plot(pca$sdev^2, type = 'l', lwd = 1.5)
abline(v = 6, col = "red", lty = 2, lwd = 2)
abline(v = 13, col = "red", lty = 2, lwd = 2)
# Choose 6 
sum((pca$sdev^2)[1:6])/sum(pca$sdev^2) # 61%

# 70% - 8 PCs
sum((pca$sdev^2)[1:8])/sum(pca$sdev^2)

# Broken stick model
p = length(pca$sdev)
broken_stick = rep(NA, p)
for(k in 1:p){
  val = 0
  for(i in k:p){
    val = val + 1/i
  }
  broken_stick[k] = val
}
# How many eigenvalues exceed their broken stick equivalent?
sum(broken_stick<(pca$sdev^2)) # 8 - 70%

# Plotting just the first 2 (31%)
coords <- cbind(Sites = train$Sites, as.data.frame(as.matrix(train[2:40])%*%pca$rotation[,1:13]))
ggplot(coords, aes(x = PC1, y = PC2, label=Sites)) +
  geom_point() + 
  geom_text(hjust = -0.25)

# colour by presence of fish at a site
coords_fish = cbind(coords, fish_binary)

# Heuningnes Redfin
ggplot(coords_fish, aes(x = PC1, y = PC2, label=Sites)) + 
  geom_point(aes(col = `Heuningnes Redfin`)) + 
  geom_text(hjust = -0.25, aes(col = `Heuningnes Redfin`))

# Cape Kurper
ggplot(coords_fish, aes(x = PC1, y = PC2, label=Sites)) + 
  geom_point(aes(col = `Cape Kurper`)) + 
  geom_text(hjust = -0.25, aes(col = `Cape Kurper`))

# Cape Galaxias
ggplot(coords_fish, aes(x = PC1, y = PC2, label=Sites)) + 
  geom_point(aes(col = `Cape Galaxias`)) + 
  geom_text(hjust = -0.25, aes(col = `Cape Galaxias`))
# cluster well - low pc1 (<50)
pca$rotation[,1:2]
# not many sections of narrow or shallow river
# more open canopy
# no macrophages most common, followed by abundant?? weird
# undercut bank more common
# no woody debris
# silt-sand substrate
# lower values for slope, flow, elevation
# water quality tricky to make sweeping statements about

# Spotted Bass
ggplot(coords_fish, aes(x = PC1, y = PC2, label=Sites)) + 
  geom_point(aes(col = `Spotted Bass`)) + 
  geom_text(hjust = -0.25, aes(col = `Spotted Bass`))
# low PC1, close to 0 PC2

# Bluegill Sunfish
ggplot(coords_fish, aes(x = PC1, y = PC2, label=Sites)) + 
  geom_point(aes(col = `Bluegill Sunfish`)) + 
  geom_text(hjust = -0.25, aes(col = `Bluegill Sunfish`))
# low PC1, close to 0 PC2

PCAbiplot(train[2:40], scaled.mat=T, samples=list(label=T))
#' Not much is obvious with just the 2 PCs
#' This makes sense, since we are losing so much information from the data
#' Need at least 8PCs - broken stick
#' I would start by building an mvabund model with the first 8PCs. 
#' If this doesn't perform well, try 13

################################################################################
# mvabund first attempt
################################################################################










