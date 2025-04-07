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

# Common Carp must be removed
# Remove binary vars while I'm at it
cc_col = which(names(data_rf) == "Common Carp")
data_rf  = data_rf[,-c(cc_col, cc_col+1, cc_col+2)]

################################################################################
# Correlations
################################################################################
corrplot(cor(data_rf[,2:47]), method = "color", type = "upper", 
         diag = FALSE,  
         tl.col = "black", tl.srt = 35, addCoef.col = "black",
         tl.cex = 0.4, number.cex = 0.6)
#' Spotted bass may be well predicted by undercut riverbanks
#' Cape galaxias have a number of relatively strong pos and neg correlations, 
#' particularly with water quality vars
corrplot(cor(data_rf[,2:47]), method = "color", type = "upper", 
         diag = FALSE,  
         tl.col = "black", tl.srt = 45, 
         tl.cex = 1)

corrplot(cor(data_rf[,43:47]), method = "color", type = "upper", 
         diag = FALSE,  
         tl.col = "black", tl.srt = 45, 
         tl.cex = 1, , addCoef.col = "black")

# citation("usdm")
# install.packages("usdm")
library(usdm)
# remove highly correlated variables
vif_res = vifstep(data_rf[,2:42])
remove = which(colnames(data_rf) %in% vif_res@excluded)
# subset of variables without extreme correlations
colnames(data_rf)
data_rf_nocor = data_rf[,-(remove)]
# ncol(data_rf_nocor)
corrplot(cor(data_rf_nocor[,2:33]), method = "color", type = "upper", 
         diag = FALSE,  
         tl.col = "black", tl.srt = 45, 
         tl.cex = 1)
max(abs((cor(data_rf_nocor[,2:33])<1)*cor(data_rf_nocor[,2:33])))

################################################################################
# Nonlinear MDS
################################################################################
# dismat <- dist(data_rf[,2:42], 
#                method = "euclidean", 
#                diag = TRUE, 
#                upper = TRUE)
# 
# coords <- metaMDS(dismat, k = 2) |> scores()
# coords <- cbind(Sites = data_rf$Sites, as.data.frame(coords))
# ggplot(coords, aes(x = NMDS1, y = NMDS2, label=Sites)) +
#   geom_point() + 
#   geom_text(hjust = -0.25)
# 
# hier_clust <- dismat |> 
#   agnes(diss = TRUE, method = "average")
# 
# # Plot
# pltree(hier_clust, main = "Dendrogram") 
# 
# # Assuming obs 43 has been removed
# num_clusts = 2:(nrow(data_rf)-1)
# # Silhouette widths
# out <- sapply(num_clusts, 
#               FUN = function(k){mean(silhouette(cutree(hier_clust, 
#                                                        k = k), 
#                                                 dismat)[,3])})
# sc <- max(out) # Best silhouette width
# best_k <- num_clusts[which(out == sc)] # Associated k
# 
# # Cut tree to have 2 clusters 
# hier_cut2 <- cutree(hier_clust, k = 2)
# 
# # Plot in 2D
# hier_clust_data <- cbind(clust = as.factor(hier_cut2), as.data.frame(coords))
# 
# ggplot(hier_clust_data, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
#   geom_point(aes(col = clust)) + 
#   geom_text(hjust = -0.25,aes(col = clust))
# 
# colour by presence of fish at a site
fish_binary = apply(data_rf[,43:47], MARGIN = c(1, 2), FUN = function(x){if(x>0)
  return(as.factor(1))
  else{return(as.factor(0))}})
coords_fish = cbind(coords, fish_binary)
# 
# # Heuningnes Redfin
# ggplot(coords_fish, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
#   geom_point(aes(col = `Heuningnes Redfin`)) + 
#   geom_text(hjust = -0.25, aes(col = `Heuningnes Redfin`))
# 
# # Cape Kurper
# ggplot(coords_fish, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
#   geom_point(aes(col = `Cape Kurper`)) + 
#   geom_text(hjust = -0.25, aes(col = `Cape Kurper`))
# 
# # Cape Galaxias
# ggplot(coords_fish, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
#   geom_point(aes(col = `Cape Galaxias`)) + 
#   geom_text(hjust = -0.25, aes(col = `Cape Galaxias`))
# # cluster well
# 
# # Spotted Bass
# ggplot(coords_fish, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
#   geom_point(aes(col = `Spotted Bass`)) + 
#   geom_text(hjust = -0.25, aes(col = `Spotted Bass`))
# # low NMDS2, high NMDS1
# 
# # Bluegill Sunfish
# ggplot(coords_fish, aes(x = NMDS1, y = NMDS2, label=Sites)) + 
#   geom_point(aes(col = `Bluegill Sunfish`)) + 
#   geom_text(hjust = -0.25, aes(col = `Bluegill Sunfish`))
# # low NMDS2, high NMDS1

################################################################################
# Linear PCA
################################################################################
pca = prcomp(data_rf[,2:42], retx = TRUE, center = TRUE, scale = TRUE)
# ?prcomp

# Choose 12
sum((pca$sdev^2)[1:sum(pca$sdev^2>1)])/sum(pca$sdev^2) # 79%

plot(pca$sdev^2, type = 'l', lwd = 1.5)
abline(v = 8, col = "red", lty = 2, lwd = 2)
abline(v = 12, col = "red", lty = 2, lwd = 2)
# Choose 6 
sum((pca$sdev^2)[1:8])/sum(pca$sdev^2) # 66.5%

# 70% - 9 PCs
sum((pca$sdev^2)[1:9])/sum(pca$sdev^2)

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
sum(broken_stick<(pca$sdev^2)) # 8 - 66.5%
xtable(as.data.frame(as.matrix(data_rf[2:42])%*%pca$rotation[,1:8]))

# Plotting just the first 2 (31%)
coords <- cbind(Sites = data_rf$Sites, as.data.frame(as.matrix(data_rf[2:42])%*%pca$rotation[,1:13]))
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

# PCAbiplot(data_rf[2:40], scaled.mat=T, samples=list(label=T))
#' Not much is obvious with just the 2 PCs
#' This makes sense, since we are losing so much information from the data
#' Need at least 6PCs - broken stick
#' I would start by building an mvabund model with the first 8PCs. 
#' If this doesn't perform well, try 13

# Interpret first 6 PCs
pcs <- as.data.frame(pca$rotation[,1:8])
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
# Kernel PCA
################################################################################
# library(kernlab)
# mat_rf  = as.matrix(data_rf[2:42])
# kpca_rf = kpca(x = mat_rf, kernel = "rbfdot", kpar = list(sigma = 0.0001),
#      features = 15, th = 1e-4)
# # pcv(kpca_rf)
# eig(kpca_rf)
# # As sigma gets smaller ie. closer to linear pca, eig get larger
# 
# # Laplace
# kpca_rf = kpca(x = mat_rf, kernel = "laplacedot", kpar = list(sigma = 0.019),
#                features = 15, th = 1e-4)
# # pcv(kpca_rf)
# eig(kpca_rf)

# Spline
# kpca_rf = kpca(x = mat_rf, kernel = "splinedot",
#                features = 15, th = 1e-4)
# # pcv(kpca_rf)
# eig(kpca_rf)
################################################################################
# Model building
################################################################################
library(mvabund)
# Create mvabund objects
# data_rf_mvabund = mvabund(data_rf[,43:47])
pcscores_mvabund = mvabund(coords)
# Investigate mean-variance relationship
meanvar.plot(mvabund(data_rf[,2:47]), 
             cex = 1, 
             pch = 19, 
             cex.axis = 0.9, 
             ylab = "Variance", 
             xlab = "Mean") # strong mean-variance relationship

# Fix names
colnames(data_rf) = gsub("-", ".", gsub(" ", ".", colnames(data_rf)))
colnames(data_rf)[35:42] = as.data.frame(strsplit(colnames(data_rf)[35:42], split = "th"))[1,]

# FULL MODEL
set.seed(1)
colnames(data_rf)
mod1 <- manyglm(formula = as.mvabund(data_rf[,43:47]) ~ Latitude + 
                  Longitude + 
                  pH + 
                  EC +
                  DO + 
                  Temp + 
                  Ammonia + 
                  Phosphorous + 
                  Nitrite + 
                  Nitrate + 
                  Total_Iron + 
                  Phosphonate + 
                  TDS + 
                  Inorg_Nitrogen +
                  Elevation + 
                  Flow + 
                  Slope + 
                  Silt.Sand.Substrate + 
                  Gravel.Substrate + 
                  Cobble.Substrate +
                  Boulder.Substrate + 
                  Bedrock.Substrate + 
                  Woody.Debris + 
                  No.Woody.Debris + 
                  Undercut.Bank + 
                  No.Undercut.Bank + 
                  No.Macrophytes + 
                  Scarce.Macrophytes + 
                  Moderate.Macrophytes + 
                  Abundant.Macrophytes + 
                  Open.Canopy + 
                  Partial.Canopy + 
                  Closed.Canopy + 
                  Shallow.Water.Dep + 
                  Moderate.Water.Dep + 
                  Deep.Water.Dep + 
                  Very.Deep.Water.Dep + 
                  Narrow.River.Wid + 
                  Moderate.River.Wid + 
                  Wide.River.Wid + 
                  Very.Wide.Wid, 
                data = data_rf,
                cor.type="shrink",
                family = "negative_binomial")
mod1_plot = plot(mod1, subtitle = NULL) #Yes
# ggsave("plots/mod1.png", plot = mod4_plot, width = 6, height = 4)
# Test for treatment effects
# an_mod1_unadj = anova(mod1, p.uni = "unadjusted", cor.type = "shrink", test = "wald", resamp = "montecarlo")
# saveRDS(an_mod1_unadj, "an_mod1_unadj.RDS")
an_mod1_unadj = readRDS("data/an_mod1_unadj.RDS")
# an_mod1_adj   = anova(mod1, p.uni = "adjusted", cor.type = "shrink", test = "wald", resamp = "montecarlo")
# saveRDS(an_mod1_adj, "an_mod1_adj.RDS")
an_mod1_adj = readRDS("data/an_mod1_adj.RDS")
# summ_mod1 = summary(mod1)
# saveRDS(summ_mod1, "summ_mod1.RDS")
summ_mod1 = readRDS("data/summ_mod1.RDS")

# xtable(an_mod1$table)
# xtable(an_mod1$uni.p)

# VIF ADJUSTED ONLY
set.seed(1)
colnames(data_rf_nocor) = colnames(data_rf)[-(remove)]

mod2 <- manyglm(formula = as.mvabund(data_rf[,43:47]) ~ Latitude + 
                  pH + 
                  EC +
                  DO + 
                  Temp + 
                  Ammonia + 
                  Phosphorous + 
                  Nitrite + 
                  Nitrate + 
                  Total_Iron + 
                  Phosphonate + 
                  TDS + 
                  Slope + 
                  Gravel.Substrate + 
                  Cobble.Substrate +
                  Boulder.Substrate + 
                  Bedrock.Substrate +
                  No.Woody.Debris + 
                  Undercut.Bank + 
                  No.Undercut.Bank + 
                  Scarce.Macrophytes + 
                  Moderate.Macrophytes + 
                  Abundant.Macrophytes + 
                  Partial.Canopy + 
                  Closed.Canopy + 
                  Moderate.Water.Dep + 
                  Deep.Water.Dep + 
                  Very.Deep.Water.Dep + 
                  Narrow.River.Wid + 
                  Moderate.River.Wid +
                  Very.Wide.Wid, 
                family = "negative_binomial", 
                data = data_rf,
                cor.type="shrink")

mod2_plot = plot(mod2, subtitle = NULL) #Yes
# ggsave("plots/mod1.png", plot = mod4_plot, width = 6, height = 4)
# Test for treatment effects
# an_mod2_unadj = anova(mod2, p.uni = "unadjusted", cor.type = "shrink", test = "wald", resamp = "montecarlo")
# saveRDS(an_mod2_unadj, "an_mod2_unadj.RDS")
an_mod2_unadj = readRDS("data/an_mod2_unadj.RDS")
# an_mod2_adj   = anova(mod2, p.uni = "adjusted", cor.type = "shrink", test = "wald", resamp = "montecarlo")
# saveRDS(an_mod2_adj, "an_mod2_adj.RDS")
an_mod2_adj = readRDS("data/an_mod2_adj.RDS")
# summ_mod2 = summary(mod2)
# saveRDS(summ_mod2, "summ_mod2.RDS")
summ_mod2 = readRDS("data/summ_mod2.RDS")

# xtable(an_mod1$table)
# xtable(an_mod1$uni.p)

# BEST SUBSET REGRESSION
#' best_bic = rep(Inf, 41) # where index indicates number of expl variables in model
#' best_comb = matrix(0, nrow = 41, ncol = 41)
expl_vars = data_rf[,2:42]
#' for(i in 41:1){
#'   print(i)
#'   # build model with every combination of i variables
#'   # i.e. when i = 41, we will be building a full model
#'   # when i = 1, we will be building a model with 1 expl variable
#'   mods = combn(1:41, i)
#'   # build each model
#'   for(j in 1:ncol(mods)){
#'     current_mod = manyglm(as.mvabund(data_rf[,43:47]) ~ as.matrix(expl_vars[,mods[,j]]), 
#'                           family = "negative_binomial",
#'                           cor.type="shrink")
#'     current_bic = mean(BIC(current_mod))
#'     if(current_bic<best_bic[i]){
#'       best_bic[i] = current_bic
#'       best_comb[1:i,i] = mods[,j]
#'     }
#'   }
#' }
#' # saveRDS(best_bic, "data/best_bic_36_41.RDS")
#' # saveRDS(best_comb, "data/best_comb_36_41.RDS")
#' best_bic = readRDS("data/best_bic_36_41.RDS")
#' best_comb = readRDS("data/best_comb_36_41.RDS")
#' 
#' setdiff(best_comb[,41], best_comb[,36])
#' #' 19 
#' #' 22 and 32
#' #' 19 30 and 33
#' #' 20 21 28 and 33
#' #' 19 21 27 29 and 34
#' colnames(data_rf)
#' 
#' set.seed(1)
#' mod3 <- manyglm(as.mvabund(data_rf[,43:47]) ~ , 
#'                 family = "negative_binomial", 
#'                 theta.method = "PHI", 
#'                 maxiter2 = 10)
#' mod3_plot = plot(mod3, subtitle = NULL) #Yes
#' # ggsave("plots/mod3.png", plot = mod3_plot, width = 6, height = 4)
#' # Test for treatment effects
#' # an_mod3_unadj = anova(mod3, p.uni = "unadjusted", resamp = "montecarlo")
#' # saveRDS(an_mod3_unadj, "an_mod3_unadj.RDS")
#' an_mod3_unadj = readRDS("an_mod3_unadj.RDS")
#' # an_mod3_adj   = anova(mod3, p.uni = "adjusted", resamp = "montecarlo")
#' # saveRDS(an_mod3_adj, "an_mod3_adj.RDS")
#' an_mod3_adj = readRDS("an_mod3_adj.RDS")
#' # summ_mod3 = summary(mod3)
#' # saveRDS(summ_mod3, "summ_mod3.RDS")
#' summ_mod3 = readRDS("summ_mod3.RDS")

# xtable(an_mod3$table)
# xtable(an_mod3$uni.p)

# BACKWARDS SELECTION
best_bic = rep(Inf, 41) # where index indicates number of expl variables in model
best_rmv = rep(NA, 41)
for(i in 41:1){
  print(i)
  # remove variables one at a time, maximising BIC 
  # mods = combn(remaining, i)
  # build each model
  vars = setdiff(1:41, best_rmv)
  mods = combn(vars, length(vars)-1)
  for(j in 1:ncol(mods)){
    # remove variable j
    current_mod = manyglm(as.mvabund(data_rf[,43:47]) ~ as.matrix(expl_vars[,mods[,j]]), 
                          family = "negative_binomial",
                          cor.type="shrink")
    current_bic = mean(BIC(current_mod)[1:2])
    if(current_bic<best_bic[i]){
      best_bic[i] = current_bic
      best_rmv[i] = setdiff(vars, mods[,j])
    }
  }
}
# saveRDS(best_bic, "data/best_bic_back.RDS")
# saveRDS(best_rmv, "data/best_rmv_back.RDS")
best_bic_back = readRDS("data/best_bic_back.RDS")
best_rmv_back = readRDS("data/best_rmv_back.RDS")
plot(rev(best_bic_back), xlab = "Number Vars Removed", 
     ylab = "BIC", 
     type = "l", 
     cex.lab = 2, 
     cex.axis = 1.5, 
     lwd = 2, 
     xaxt = "n")
axis(1, at = seq(0, 40, by = 5), labels = seq(0, 40, by = 5), cex.axis = 1.5)

# abline(v = 42-28)
# abline(v = 42-23)
# abline(v = 42-15)
# abline(v = 42-10)
# abline(v = 42-5)
# setdiff(1:41, best_rmv)

#' 19 
#' 22 and 32
#' 19 30 and 33
#' 20 21 28 and 33
#' 19 21 27 29 and 34
# colnames(data_rf)

# set.seed(1)
# mod3 <- manyglm(data_rf_mvabund[,43:47] ~ as.matrix(expl_vars[,c(3, 15, 2, 31, 40, 39, 12)]), 
#                 family = "negative_binomial", 
#                 theta.method = "PHI", 
#                 maxiter2 = 10)
# mean(BIC(mod3))
# mod3_plot = plot(mod3, subtitle = NULL) #Yes
# ggsave("plots/mod3.png", plot = mod3_plot, width = 6, height = 4)
# Test for treatment effects
# an_mod3_unadj = anova(mod3, p.uni = "unadjusted", resamp = "montecarlo")
# saveRDS(an_mod3_unadj, "an_mod3_unadj.RDS")
an_mod3_unadj = readRDS("data/an_mod3_unadj.RDS")
# an_mod3_adj   = anova(mod3, p.uni = "adjusted", resamp = "montecarlo")
# saveRDS(an_mod3_adj, "an_mod3_adj.RDS")
an_mod3_adj = readRDS("data/an_mod3_adj.RDS")
# summ_mod3 = summary(mod3)
# saveRDS(summ_mod3, "summ_mod3.RDS")
summ_mod3 = readRDS("data/summ_mod3.RDS")

mod3.5 <- manyglm(as.mvabund(data_rf[,43:47]) ~ pH +
                      Elevation +
                      Longitude +
                      Open.Canopy + 
                      Wide.River.Wid, 
                    data = data_rf[,2:42],
                    family = "negative_binomial",
                  cor.type="shrink")
plot(mod3.5, subtitle = NULL) 
# an_mod3.5_adj   = anova(mod3.5, p.uni = "adjusted", resamp = "montecarlo")
# saveRDS(an_mod3.5_adj, "an_mod3.5_adj.RDS")
an_mod3.5_adj = readRDS("data/an_mod3.5_adj.RDS")
xtable(coef(mod3.5), digits = 3)
data_rf$Heuningnes.Redfin
sqrt(mean((predict.manyglm(mod3.5, type = "response")[,1]-data_rf$Heuningnes.Redfin)^2))
sqrt(mean((predict.manyglm(mod3.5, type = "response")[,2]-data_rf$Cape.Kurper)^2))
sqrt(mean((predict.manyglm(mod3.5, type = "response")[,3]-data_rf$Cape.Galaxias)^2))
sqrt(mean((predict.manyglm(mod3.5, type = "response")[,4]-data_rf$Spotted.Bass)^2))
sqrt(mean((predict.manyglm(mod3.5, type = "response")[,5]-data_rf$Bluegill.Sunfish)^2))
# summ_mod3.5 = summary(mod3.5)
# saveRDS(summ_mod3.5, "summ_mod3.5.RDS")
# summ_mod3.5 = readRDS("summ_mod3.5.RDS")
# ?anova.manyglm
min(data_rf$Longitude)-max(data_rf$Longitude)



mod3.10 <- manyglm(as.mvabund(data_rf[,43:47]) ~ pH +
                       Elevation +
                       Longitude +
                       Open.Canopy + 
                       Wide.River.Wid + 
                       Moderate.River.Wid +
                       Phosphonate +
                       Very.Wide.Wid + 
                       Narrow.River.Wid + 
                       EC, 
                     data = data_rf[,2:42],
                     family = "negative_binomial",
                   cor.type="shrink")
plot(mod3.10, subtitle = NULL) 

mod3.15 <- manyglm(as.mvabund(data_rf[,43:47]) ~ pH +
                       Elevation +
                       Longitude +
                       Open.Canopy + 
                       Wide.River.Wid + 
                       Moderate.River.Wid +
                       Phosphonate +
                       Very.Wide.Wid + 
                       Narrow.River.Wid + 
                       EC +
                       Gravel.Substrate + 
                       Total_Iron + 
                       No.Macrophytes + 
                       Scarce.Macrophytes + 
                       Inorg_Nitrogen, 
                     data = data_rf[,2:42],
                     family = "negative_binomial",
                   cor.type="shrink")
plot(mod3.15, subtitle = NULL) 

mod3.23 <- manyglm(as.mvabund(data_rf[,43:47]) ~ pH +
                       Elevation +
                       Longitude +
                       Open.Canopy + 
                       Wide.River.Wid + 
                       Moderate.River.Wid +
                       Phosphonate +
                       Very.Wide.Wid + 
                       Narrow.River.Wid + 
                       EC +
                       Gravel.Substrate + 
                       Total_Iron + 
                       No.Macrophytes + 
                       Scarce.Macrophytes + 
                       Inorg_Nitrogen + 
                       Nitrite + 
                       DO + 
                       Flow + 
                       Silt.Sand.Substrate + 
                       Closed.Canopy + 
                       Slope + 
                       Bedrock.Substrate + 
                       Nitrate, 
                     data = data_rf[,2:42],
                     family = "negative_binomial",
                   cor.type="shrink")
plot(mod3.23, subtitle = NULL) 

mod3.28 <- manyglm(as.mvabund(data_rf[,43:47]) ~ pH +
                       Elevation +
                       Longitude +
                       Open.Canopy + 
                       Wide.River.Wid + 
                       Moderate.River.Wid +
                       Phosphonate +
                       Very.Wide.Wid + 
                       Narrow.River.Wid + 
                       EC +
                       Gravel.Substrate + 
                       Total_Iron + 
                       No.Macrophytes + 
                       Scarce.Macrophytes + 
                       Inorg_Nitrogen + 
                       Nitrite + 
                       DO + 
                       Flow + 
                       Silt.Sand.Substrate + 
                       Closed.Canopy + 
                       Slope + 
                       Bedrock.Substrate + 
                       Nitrate + 
                       Woody.Debris + 
                       Temp + 
                       Shallow.Water.Dep + 
                       Abundant.Macrophytes + 
                       Undercut.Bank, 
                     data = data_rf[,2:42],
                     family = "negative_binomial",
                   cor.type="shrink")
plot(mod3.28, subtitle = NULL) 


# colnames(expl_vars[best_rmv_back[2:41]])
# PRINCIPAL COMPONENTS
#' for multivariate abundance data it has been shown that the negative binomial 
#' distribution (family="negative.binomial") is usually a better choice 
#' (Warton 2005)
set.seed(1)

mod4 <- manyglm(as.mvabund(data_rf[,43:47]) ~ coords$PC1 + 
                  coords$PC2 + 
                  coords$PC3 + 
                  coords$PC4 + 
                  coords$PC5 + 
                  coords$PC6 +
                  coords$PC7 +
                  coords$PC8, 
                family = "negative_binomial",
                cor.type="shrink")

mod4_plot = plot(mod4, subtitle = NULL) #Yes
# ggsave("plots/mod4.png", plot = mod4_plot, width = 6, height = 4)
# Test for treatment effects
# an_mod4_unadj = anova(mod4, p.uni = "unadjusted", resamp = "montecarlo")
# saveRDS(an_mod4_unadj, "data/an_mod4_unadj.RDS")
an_mod4_unadj = readRDS("data/an_mod4_unadj.RDS")
# an_mod4_adj   = anova(mod4, p.uni = "adjusted", resamp = "montecarlo")
# saveRDS(an_mod4_adj, "data/an_mod4_adj.RDS")
an_mod4_adj = readRDS("data/an_mod4_adj.RDS")
# summ_mod4 = summary(mod4)
# saveRDS(summ_mod4, "data/summ_mod4.RDS")
summ_mod4 = readRDS("data/summ_mod4.RDS")

# xtable(an_mod4$table)
# xtable(an_mod4$uni.p)

################################################################################
# Map
################################################################################
# install.packages("leaflet")
library(leaflet)

long_lat <- data_loc[,c(1, 4, 3)]
colnames(long_lat) = c("Site", "lng", "lat")
as.matrix(long_lat)[55,]
leaflet() %>%
  addTiles() %>%
  setView(lng = 19.83141, lat = -34.425, zoom = 8)%>%
  addMarkers(data = long_lat[,2:3], label = long_lat$Site)

leaflet() %>%
  addTiles() %>%
  setView(lng = 19.83141, lat = -34.425, zoom = 12)%>%
  addMarkers(data = long_lat[,2:3], label = long_lat$Site, 
             labelOptions = c(permanent = FALSE))
# ?addMarkers
# summary(long_lat)
# ?manyglm
################################################################################
# GLLVM
################################################################################
library(gllvm)
data_rf_new = data_rf
colnames(data_rf)[34]

gllvm(y = as.matrix(data_rf_new[,43:47]), X = scale(as.matrix(data_rf_new[,2:42])), family = "negative.binomial")

det(var(data_rf_new[,2:19]))
################################################################################
# Cross Validation - determine final model
################################################################################
# 14-fold cross validation - set seed to 8, only do 13 folds - 
# -> issue with pca: cannot rescale a constant/zero column to unit variance
# 7 fold cv - set seed to 64
set.seed(8)
nmod = 3 # number of models
nfold = 14 # Number of folds
fold_size = 56/nfold # number of obs per fold
samp = sample(56, 56)
RMSE = as.data.frame(matrix(NA, nrow = nfold*nmod, ncol = 3))
colnames(RMSE) = c("Model", "Fold", "RMSE")

# For each fold
for(i in 1:13){
  # split into training and test for this fold
  test  = data_rf[samp[(i*fold_size-3):(i*fold_size)],]
  train = data_rf[samp[-((i*fold_size-3):(i*fold_size))],]
  # sum(train$Spotted.Bass>1)
  # build models on training set
  # mod1_cv = manyglm(formula = as.mvabund(train[,43:47]) ~ Latitude + 
  #                     Longitude + 
  #                     pH + 
  #                     EC +
  #                     DO + 
  #                     Temp + 
  #                     Ammonia + 
  #                     Phosphorous + 
  #                     Nitrite + 
  #                     Nitrate + 
  #                     Total_Iron + 
  #                     Phosphonate + 
  #                     TDS + 
  #                     Inorg_Nitrogen +
  #                     Elevation + 
  #                     Flow + 
  #                     Slope + 
  #                     Silt.Sand.Substrate + 
  #                     Gravel.Substrate + 
  #                     Cobble.Substrate +
  #                     Boulder.Substrate + 
  #                     Bedrock.Substrate + 
  #                     Woody.Debris + 
  #                     No.Woody.Debris + 
  #                     Undercut.Bank + 
  #                     No.Undercut.Bank + 
  #                     No.Macrophytes + 
  #                     Scarce.Macrophytes + 
  #                     Moderate.Macrophytes + 
  #                     Abundant.Macrophytes + 
  #                     Open.Canopy + 
  #                     Partial.Canopy + 
  #                     Closed.Canopy + 
  #                     Shallow.Water.Dep + 
  #                     Moderate.Water.Dep + 
  #                     Deep.Water.Dep + 
  #                     Very.Deep.Water.Dep + 
  #                     Narrow.River.Wid + 
  #                     Moderate.River.Wid + 
  #                     Wide.River.Wid + 
  #                     Very.Wide.Wid, 
  #                   data = train[,2:42], 
  #                   family = "negative_binomial",
  #                   cor.type="shrink")
  # mod2_cv = manyglm(formula = as.mvabund(train[,43:47]) ~ Latitude + 
  #                     pH + 
  #                     EC +
  #                     DO + 
  #                     Temp + 
  #                     Ammonia + 
  #                     Phosphorous + 
  #                     Nitrite + 
  #                     Nitrate + 
  #                     Total_Iron + 
  #                     Phosphonate + 
  #                     TDS + 
  #                     Slope + 
  #                     Gravel.Substrate + 
  #                     Cobble.Substrate +
  #                     Boulder.Substrate + 
  #                     Bedrock.Substrate +
  #                     No.Woody.Debris + 
  #                     Undercut.Bank + 
  #                     No.Undercut.Bank + 
  #                     Scarce.Macrophytes + 
  #                     Moderate.Macrophytes + 
  #                     Abundant.Macrophytes + 
  #                     Partial.Canopy + 
  #                     Closed.Canopy + 
  #                     Moderate.Water.Dep + 
  #                     Deep.Water.Dep + 
  #                     Very.Deep.Water.Dep + 
  #                     Narrow.River.Wid + 
  #                     Moderate.River.Wid +
  #                     Very.Wide.Wid,
  #                   data = train[,2:42], 
  #                   family = "negative_binomial",
  #                   cor.type="shrink")
  
  mod3_cv5 <- manyglm(as.mvabund(train[,43:47]) ~ pH +
                       Elevation +
                       Longitude +
                       Open.Canopy + 
                       Wide.River.Wid, 
                     data = train[,2:42],
                  family = "negative_binomial",
                  cor.type="shrink")
  
  # mod3_cv10 <- manyglm(as.mvabund(train[,43:47]) ~ pH +
  #                       Elevation +
  #                       Longitude +
  #                       Open.Canopy + 
  #                       Wide.River.Wid + 
  #                       Moderate.River.Wid +
  #                       Phosphonate +
  #                       Very.Wide.Wid + 
  #                       Narrow.River.Wid + 
  #                       EC, 
  #                     data = train[,2:42],
  #                     family = "negative_binomial",
  #                     cor.type="shrink")
  
  mod3_cv15 <- manyglm(as.mvabund(train[,43:47]) ~ pH +
                         Elevation +
                         Longitude +
                         Open.Canopy + 
                         Wide.River.Wid + 
                         Moderate.River.Wid +
                         Phosphonate +
                         Very.Wide.Wid + 
                         Narrow.River.Wid + 
                         EC +
                         Gravel.Substrate + 
                         Total_Iron + 
                         No.Macrophytes + 
                         Scarce.Macrophytes + 
                         Inorg_Nitrogen, 
                       data = train[,2:42],
                       family = "negative_binomial",
                       cor.type="shrink")
  
  # mod3_cv23 <- manyglm(as.mvabund(train[,43:47]) ~ pH +
  #                        Elevation +
  #                        Longitude +
  #                        Open.Canopy + 
  #                        Wide.River.Wid + 
  #                        Moderate.River.Wid +
  #                        Phosphonate +
  #                        Very.Wide.Wid + 
  #                        Narrow.River.Wid + 
  #                        EC +
  #                        Gravel.Substrate + 
  #                        Total_Iron + 
  #                        No.Macrophytes + 
  #                        Scarce.Macrophytes + 
  #                        Inorg_Nitrogen + 
  #                        Nitrite + 
  #                        DO + 
  #                        Flow + 
  #                        Silt.Sand.Substrate + 
  #                        Closed.Canopy + 
  #                        Slope + 
  #                        Bedrock.Substrate + 
  #                        Nitrate, 
  #                      data = train[,2:42],
  #                      family = "negative_binomial",
  #                      cor.type="shrink")
  
  # mod3_cv28 <- manyglm(as.mvabund(train[,43:47]) ~ pH +
  #                        Elevation +
  #                        Longitude +
  #                        Open.Canopy + 
  #                        Wide.River.Wid + 
  #                        Moderate.River.Wid +
  #                        Phosphonate +
  #                        Very.Wide.Wid + 
  #                        Narrow.River.Wid + 
  #                        EC +
  #                        Gravel.Substrate + 
  #                        Total_Iron + 
  #                        No.Macrophytes + 
  #                        Scarce.Macrophytes + 
  #                        Inorg_Nitrogen + 
  #                        Nitrite + 
  #                        DO + 
  #                        Flow + 
  #                        Silt.Sand.Substrate + 
  #                        Closed.Canopy + 
  #                        Slope + 
  #                        Bedrock.Substrate + 
  #                        Nitrate + 
  #                        Woody.Debris + 
  #                        Temp + 
  #                        Shallow.Water.Dep + 
  #                        Abundant.Macrophytes + 
  #                        Undercut.Bank, 
  #                      data = train[,2:42],
  #                      family = "negative_binomial",
  #                      cor.type="shrink")

  # PCA
  pca = prcomp(train[,2:42], retx = TRUE, center = TRUE, scale = TRUE)
  coords_train <- cbind(Sites = train$Sites, as.data.frame(as.matrix(train[,2:42])%*%pca$rotation[,1:8]))
  
  mod4_cv <- manyglm(as.mvabund(train[,43:47]) ~ PC1 + 
                    PC2 + 
                    PC3 + 
                    PC4 + 
                    PC5 + 
                    PC6 +
                    PC7 +
                    PC8, 
                  data = coords_train,
                  family = "negative_binomial",
                  cor.type="shrink")
  
  # evaluate performance on test set RMSE
  newdata_expl = test[,2:42]
  newdata_resp = test[,43:47]

  # Full model
  # rmse1 = sqrt(sum((predict(mod1_cv, newdata_expl, type = "response") - newdata_resp)^2)/(fold_size*5))
  # RMSE[i+nfold*0,] = c("Full", i, rmse1)
  # 
  # # VIF model
  # rmse2 = sqrt(sum((predict(mod2_cv, newdata_expl, type = "response") - newdata_resp)^2)/(fold_size*5))
  # RMSE[i+nfold*1,] = c("VIF", i, rmse2)
  
  # Stepwise model 5
  rmse3.5 = sqrt(sum((predict(mod3_cv5, newdata_expl, type = "response") - newdata_resp)^2)/(fold_size*5))
  RMSE[i+nfold*0,] = c("Step5", i, rmse3.5)
  
  # Stepwise model 10
  # rmse3.10 = sqrt(sum((predict(mod3_cv10, newdata_expl, type = "response") - newdata_resp)^2)/(fold_size*5))
  # RMSE[i+nfold*3,] = c("Step10", i, rmse3.10)
  
  # Stepwise model 15
  rmse3.15 = sqrt(sum((predict(mod3_cv15, newdata_expl, type = "response") - newdata_resp)^2)/(fold_size*5))
  RMSE[i+nfold*1,] = c("Step15", i, rmse3.15)
  
  # Stepwise model 23
  # rmse3.23 = sqrt(sum((predict(mod3_cv23, newdata_expl, type = "response") - newdata_resp)^2)/(fold_size*5))
  # RMSE[i+nfold*5,] = c("Step23", i, rmse3.23)
  # 
  # Stepwise model 28
  # rmse3.28 = sqrt(sum((predict(mod3_cv28, newdata_expl, type = "response") - newdata_resp)^2)/(fold_size*5))
  # RMSE[i+nfold*2,] = c("Step28", i, rmse3.28)

  # PCA model
  newdata_expl_pcs = cbind(Sites = test$Sites, as.data.frame(as.matrix(test[,2:42])%*%pca$rotation[,1:8]))
  rmse4 = sqrt(sum((predict(mod4_cv, newdata_expl_pcs, type = "response") - newdata_resp)^2)/(fold_size*5))
  RMSE[i+nfold*2,] = c("PCA", i, rmse4)
}

RMSE$RMSE = as.numeric(RMSE$RMSE)

################################################################################
# Goodness of fit
################################################################################
# Mean RMSE after CV
# mean(filter(RMSE, Model == "Full")$RMSE[1:12])
# mean(filter(RMSE, Model == "VIF")$RMSE)
mean(filter(RMSE, Model == "Step5")$RMSE)
# mean(filter(RMSE, Model == "Step10")$RMSE[c(1, 3:13)])
mean(filter(RMSE, Model == "Step15")$RMSE[c(1, 3:13)])
# mean(filter(RMSE, Model == "Step23")$RMSE[c(1, 3:13)])
# mean(filter(RMSE, Model == "Step28")$RMSE[c(1, 3:13)])
mean(filter(RMSE, Model == "PCA")$RMSE)

# RMSE on training set
# sqrt(sum((predict(mod1, data_rf[,2:42], type = "response") - data_rf[,43:47])^2)/(fold_size*5))
# sqrt(sum((predict(mod2, data_rf[,2:42], type = "response") - data_rf[,43:47])^2)/(fold_size*5))
mod3.5 <- manyglm(as.mvabund(data_rf[,43:47]) ~ pH +
                      Elevation +
                      Longitude +
                      Open.Canopy + 
                      Wide.River.Wid, 
                    data = data_rf[,2:42],
                    family = "negative_binomial",
                    cor.type="shrink")
sqrt(sum((predict(mod3.5, data_rf[,2:42], type = "response") - data_rf[,43:47])^2)/(56*5))
# sqrt(sum((predict(mod3.10, data_rf[,2:42], type = "response") - data_rf[,43:47])^2)/(fold_size*5))
sqrt(sum((predict(mod3.15, data_rf[,2:42], type = "response") - data_rf[,43:47])^2)/(56*5))
# sqrt(sum((predict(mod3.23, data_rf[,2:42], type = "response") - data_rf[,43:47])^2)/(fold_size*5))
# sqrt(sum((predict(mod3.28, data_rf[,2:42], type = "response") - data_rf[,43:47])^2)/(fold_size*5))
sqrt(sum((predict(mod4, coords[,2:9], type = "response") - data_rf[,43:47])^2)/(56*5))

# Mean AIC
# mean(AIC(mod1))
# mean(AIC(mod2))
mean(AIC(mod3.5))
# mean(AIC(mod3.10))
mean(AIC(mod3.15))
# mean(AIC(mod3.23))
# mean(AIC(mod3.28))
mean(AIC(mod4))

# Mean BIC
# mean(BIC(mod1))
# mean(BIC(mod2))
mean(BIC(mod3.5))
# mean(BIC(mod3.10))
mean(BIC(mod3.15))
# mean(BIC(mod3.23))
# mean(BIC(mod3.28))
mean(BIC(mod4))

# BICs
# BIC(mod1)
# BIC(mod2)
BIC(mod3.5)
# BIC(mod3.10)
BIC(mod3.15)
# BIC(mod3.23)
# BIC(mod3.28)
BIC(mod4)

################################################################################
# Factor Analysis?
################################################################################
# Choose 8 (broken stick - same eigs as pca)
svd.out<- svd(cor(data_rf[,2:42]))
gamma.mat <- svd.out$v[,1:8] %*% diag(sqrt(svd.out$d[1:8]))
rownames(gamma.mat) <- colnames(data_rf[,2:42])
gamma.mat <- varimax(gamma.mat)$loadings
fa_mat = matrix(gamma.mat,ncol=8)

plot(gamma.mat[,2]~gamma.mat[,1], pch = 19)
text(gamma.mat[,2]~gamma.mat[,1], labels = rownames(gamma.mat), cex = 0.75)

plot(gamma.mat[,4]~gamma.mat[,3], pch = 19)
text(gamma.mat[,4]~gamma.mat[,3], labels = rownames(gamma.mat), cex = 0.75)

plot(gamma.mat[,6]~gamma.mat[,5], pch = 19)
text(gamma.mat[,6]~gamma.mat[,5], labels = rownames(gamma.mat), cex = 0.75)


library(rgl)
# Create 3D scatter plot
plot3d(gamma.mat[,1], gamma.mat[,2], gamma.mat[,3], 
       pty = 19)
text3d(gamma.mat[,1], gamma.mat[,2], gamma.mat[,3], 
       texts = rownames(gamma.mat), adj = c(0.5, 0.5), 
       cex = 0.5)

# mod3.41 <- manyglm(as.mvabund(data_rf[,43:47]) ~ pH +
#                      Elevation +
#                      Longitude +
#                      Open.Canopy +
#                      Wide.River.Wid +
#                      Moderate.River.Wid +
#                      Phosphonate +
#                      Very.Wide.Wid +
#                      Narrow.River.Wid +
#                      EC +
#                      Gravel.Substrate +
#                      Total_Iron +
#                      No.Macrophytes +
#                      Scarce.Macrophytes +
#                      Inorg_Nitrogen +
#                      Nitrite +
#                      DO +
#                      Flow +
#                      Silt.Sand.Substrate +
#                      Closed.Canopy +
#                      Slope +
#                      Bedrock.Substrate +
#                      Nitrate +
#                      Woody.Debris +
#                      Temp +
#                      Shallow.Water.Dep +
#                      Abundant.Macrophytes +
#                      Undercut.Bank +
#                      Deep.Water.Dep +
#                      No.Woody.Debris +
#                      No.Undercut.Bank +
#                      Moderate.Water.Dep +
#                      Very.Deep.Water.Dep +
#                      Phosphorous +
#                      TDS +
#                      Latitude +
#                      Boulder.Substrate +
#                      Cobble.Substrate +
#                      Ammonia +
#                      Moderate.Macrophytes,
#                    data = data_rf[,2:42],
#                    family = "negative_binomial")
# sqrt(sum((predict(mod3.41, data_rf[,2:42], type = "response") - data_rf[,43:47])^2)/(fold_size*5))