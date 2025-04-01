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
data_rf  <- read_excel("data\\ZivaHub_RawData.xlsx", sheet = "Proportional Converted to %")
data_loc <- read_excel("data\\ZivaHub_RawData.xlsx", sheet = "Site Locations")
# Neaten dataframe
colnames(data_rf) <- data_rf[1,]
colnames(data_loc) <- c("Survey", "Site", "Latitude", "Longitude", "Elevation")
data_rf <- data_rf[2:nrow(data_rf),]
data_rf <- data_rf |> mutate(across(everything(), as.numeric))
data_rf <- cbind(data_loc[,3:4], data_rf)

# reorder
data_rf <- data_rf[,c(3, 1, 2, 4:ncol(data_rf))]
# Update column names so easier to work with 
colnames(data_rf)[c(12,15)] <- c("Total_Iron", "Inorg_Nitrogen")

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
corrplot(cor(train[,2:47]), method = "color", type = "upper", 
         diag = FALSE,  
         tl.col = "black", tl.srt = 35, addCoef.col = "black",
         tl.cex = 0.4, number.cex = 0.6)
#' Spotted bass may be well predicted by undercut riverbanks
#' Cape galaxias have a number of relatively strong pos and neg correlations, 
#' particularly with water quality vars
corrplot(cor(train[,2:47]), method = "color", type = "upper", 
         diag = FALSE,  
         tl.col = "black", tl.srt = 45, 
         tl.cex = 1)

corrplot(cor(train[,43:47]), method = "color", type = "upper", 
         diag = FALSE,  
         tl.col = "black", tl.srt = 45, 
         tl.cex = 1, , addCoef.col = "black")

# citation("usdm")
# install.packages("usdm")
library(usdm)
# remove highly correlated variables
vif_res = vifstep(train[,2:42])
remove = which(colnames(train) %in% vif_res@excluded)
train_nocor = train[,-(remove)]
# ncol(train_nocor)
corrplot(cor(train_nocor[,2:33]), method = "color", type = "upper", 
         diag = FALSE,  
         tl.col = "black", tl.srt = 45, 
         tl.cex = 1.3)

################################################################################
# Nonlinear MDS
################################################################################
dismat <- dist(train[,2:42], 
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
fish_binary = apply(train[,43:47], MARGIN = c(1, 2), FUN = function(x){if(x>0) 
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
pca = prcomp(train[2:42], retx = TRUE, center = TRUE, scale = TRUE)
# ?prcomp

# Choose 13
sum((pca$sdev^2)[1:sum(pca$sdev^2>1)])/sum(pca$sdev^2) # 86%

plot(pca$sdev^2, type = 'l', lwd = 1.5)
abline(v = 7, col = "red", lty = 2, lwd = 2)
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
sum(broken_stick<(pca$sdev^2)) # 6 - 61%

# Plotting just the first 2 (31%)
coords <- cbind(Sites = train$Sites, as.data.frame(as.matrix(train[2:42])%*%pca$rotation[,1:13]))
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

# PCAbiplot(train[2:40], scaled.mat=T, samples=list(label=T))
#' Not much is obvious with just the 2 PCs
#' This makes sense, since we are losing so much information from the data
#' Need at least 6PCs - broken stick
#' I would start by building an mvabund model with the first 8PCs. 
#' If this doesn't perform well, try 13

# Interpret first 6 PCs
pcs <- as.data.frame(pca$rotation[,1:6])
cutoff <- 0.1
pc <- 5

# Strong Positive Impact
strong_pos <- which(pcs[,pc] >= quantile(pcs[,pc], probs = 0.8)) 
rownames(pcs)[strong_pos]
# (pcs)[strong_pos,pc]

# Strong Negative Impact
strong_neg <- which(pcs[,pc] < quantile(pcs[,pc], probs = 0.2))
rownames(pcs)[strong_neg]
# (pcs)[strong_neg,pc]

# Positive Impact
pos <- which(pcs[,pc] >= quantile(pcs[,pc], probs = 0.7) & pcs[,pc] < quantile(pcs[,pc], probs = 0.8))
rownames(pcs)[pos]
(pcs)[pos,pc]

# Negative Impact
neg <- which(pcs[,pc] <= quantile(pcs[,pc], probs = 0.3) & pcs[,pc] > quantile(pcs[,pc], probs = 0.2))
rownames(pcs)[neg]
(pcs)[neg,pc]


################################################################################
# mvabund first attempt
################################################################################
library(mvabund)
# ?meanvar.plot
train_mvabund = mvabund(train)
pcscores_mvabund = mvabund(coords)
?meanvar.plot
meanvar.plot(mvabund(train[,2:47]), 
             cex = 1, 
             pch = 19, 
             cex.axis = 0.9, 
             ylab = "Variance", 
             xlab = "Mean") # strong mean-variance relationship
# i.e. high means = high variances

# response is count data, so poisson or neg binomial
# plot(train_mvabund[,43:47] ~ as.factor(train$`Undercut Bank`), cex.axis = 0.8, cex = 0.8)
# mod1 <- manyglm(train_mvabund[,43:47] ~ train_mvabund[,2:42], family = "poisson")
# plot(mod1) # doesn't look random

# mod2 <- manyglm(train_mvabund[,43:47] ~ train_mvabund[,2:42], family = "negative_binomial")
# plot(mod2) # still not random

# Should we use PCs?
# mod3 <- manyglm(train_mvabund[,43:47] ~ pcscores_mvabund[,2:7], family = "poisson")
# plot(mod3) #Nope

#' for multivariate abundance data it has been shown that the negative binomial 
#' distribution (family="negative.binomial") is usually a better choice 
#' (Warton 2005)
set.seed(12338)
mod4 <- manyglm(train_mvabund[,43:47] ~ coords$PC1 + 
                  coords$PC2 + 
                  coords$PC3 + 
                  coords$PC4 + 
                  coords$PC5 + 
                  coords$PC6, 
                family = "negative_binomial", 
                theta.method = "PHI", 
                maxiter2 = 10)
mod4_plot = plot(mod4, subtitle = NULL) #Yes
# ggsave("plots/mod4.png", plot = mod4_plot, width = 6, height = 4)
# Test for treatment effects
# anova(mod4) # PCs definitely explain some of the var in the data
# anova(mod4, p.uni = "adjusted")
an_mod4 = anova(mod4, p.uni = "adjusted", resamp = "montecarlo")
xtable(an_mod4$table)
xtable(an_mod4$uni.p)
summary(mod4)


# What about mvabund using raw expl vars but without highly correlated vars?
train_nocor_mvabund <- as.mvabund(train_nocor)
# DOES IT MAKE ANY SENSE TO FIT A POISSON
# mod5 <- manyglm(train_mvabund[,43:47] ~ train_nocor_mvabund[,2:28],
#                 family = "poisson")
# plot(mod5) # Yes
# anova(mod5) # Vars definitely explain some of the var in the data
# anova(mod5, p.uni = "adjusted")
# summary(mod5) # MUCH BETTER!
set.seed(64)
mod6 <- manyglm(formula = train_mvabund[,43:47] ~ train_nocor_mvabund[,2:28], 
                family = "negative_binomial")
mod6_plot = plot(mod6)

library(tidyr)
?pivot_longer
fitted.values = pivot_longer(as.data.frame(mod6$fitted.values |> apply(MARGIN = 2, FUN = scale)), cols = 1:5)
colnames(fitted.values) = c("species", "fitted")
PIT.residuals = pivot_longer(as.data.frame(mod6$PIT.residuals |> apply(MARGIN = 2, FUN = scale)), cols = 1:5)
mod6_df = cbind(fitted.values, resids = PIT.residuals$value)

ggplot(mod6_df, aes(x = fitted, y = resids)) + 
  geom_point(aes())

# ggsave("plots/mod6.png", plot = mod6_plot, width = 6, height = 4)
anova(mod6) # Vars definitely explain some of the var in the data
anova(mod6, p.uni = "adjusted", resamp = "montecarlo")
summary(mod6) # Nope
# Can I set this not to assume no correlation?

#################################################
set.seed(64)
mod6 <- manyglm(formula = train_mvabund[,43:47] ~ train_nocor$Longitude + 
                  train_nocor$pH + 
                  train_nocor$DO + 
                  train_nocor$Temp + 
                  train_nocor$Phosphorous + 
                  train_nocor$Nitrite + 
                  train_nocor$Nitrate + 
                  train_nocor$Total_Iron + 
                  train_nocor$Phosphonate + 
                  train_nocor$TDS + 
                  train_nocor$Slope + 
                  train_nocor$`Gravel Substrate` + 
                  train_nocor$`Boulder Substrate` + 
                  train_nocor$`Bedrock Substrate` + 
                  train_nocor$`No Woody Debris` + 
                  train_nocor$`Undercut Bank` + 
                  train_nocor$`No Undercut Bank` + 
                  train_nocor$`Scarce Macrophytes` + 
                  train_nocor$`Moderate Macrophytes` + 
                  train_nocor$`Abundant Macrophytes` + 
                  train_nocor$`Partial Canopy` + 
                  train_nocor$`Closed Canopy` + 
                  train_nocor$`Moderate Water Depth (51-100)` + 
                  train_nocor$`Deep Water Depth (100 - 180)` + 
                  train_nocor$`Very Deep Water Depth (<180)` + 
                  train_nocor$`Moderate River Width (3 - 6)` + 
                  train_nocor$`Very Wide Width (<10)`, 
                family = "negative_binomial")
# mod6_plot = plot(mod6)
# ggsave("plots/mod6.png", plot = mod6_plot, width = 6, height = 4)
# anova(mod6) # Vars definitely explain some of the var in the data
an_mod6 = anova(mod6, p.uni = "adjusted", cor.type = "shrink", test = "wald")
sum_mod6 = summary(mod6) # Nope
# Can I set this not to assume no correlation?
?anova.manyglm
an_mod6$uni.p
xtable(an_mod6$uni.p)
#' mod5 most promising by a long shot
#' mod4 comes next
#' all others do a poor job
#' 
#' 
anova.manyglm(mod6)
?coef
xtable(coef(mod4))

set.seed(64)
mod7 <- manyglm(formula = train_mvabund[,43:47] ~ train_nocor$Longitude + 
                  train_nocor$pH + 
                  train_nocor$Temp + 
                  train_nocor$Phosphorous + 
                  train_nocor$`Deep Water Depth (100 - 180)` + 
                  train_nocor$`Very Deep Water Depth (<180)` + 
                  train_nocor$`Moderate River Width (3 - 6)` + 
                  train_nocor$`Very Wide Width (<10)`, 
                family = "negative_binomial")
mod7_plot = plot(mod7)
# ggsave("plots/mod6.png", plot = mod6_plot, width = 6, height = 4)
# anova(mod6) # Vars definitely explain some of the var in the data
an_mod7 = anova(mod7, p.uni = "adjusted", cor.type = "shrink", test = "wald")
AIC(mod7)
AIC(mod6)
sum_mod7 = summary(mod6) # Nope
# Can I set this not to assume no correlation?
an_mod7$uni.p
xtable(an_mod7$uni.p)
#' mod5 most promising by a long shot
#' mod4 comes next
#' all others do a poor job
#' 
#' 
anova.manyglm(mod7)
coef(mod7)
################################################################################
# Map
################################################################################
# install.packages("leaflet")
library(leaflet)

long_lat <- data_loc[,c(1, 4, 3)]
colnames(long_lat) = c("Site", "lng", "lat")
long_lat[54,]
leaflet() %>%
  addTiles() %>%
  setView(lng = 19.83141, lat = -34.425, zoom = 8)%>%
  addMarkers(data = long_lat[,2:3], label = long_lat$Site)

leaflet() %>%
  addTiles() %>%
  setView(lng = 19.83141, lat = -34.425, zoom = 12)%>%
  addMarkers(data = long_lat[,2:3], label = long_lat$Site, 
             labelOptions = c(permanent = FALSE))
?addMarkers
summary(long_lat)
################################################################################
# Non linear methods
################################################################################



################################################################################
# Test Set
################################################################################

?predict
predict(object = mod6, type = "response")
predict(mod6, type = "response", newdata = test[,-c(1, remove, 43:47)])


length(train_nocor)

test
train_nocor

test_pcscores = (as.matrix(test[2:42])%*%pca$rotation[,1:13])[,1:6]
predict(mod4, type = "response", newdata = as.data.frame(test_pcscores))
as.data.frame(test_pcscores)








