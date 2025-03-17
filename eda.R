
library(readxl)
library(dplyr)
library(cluster)
library(vegan)
library(ggplot2)
library(mclust)
library(xtable)

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
# Remove response var, factor/binary vars, and other species found (feels like some species may always be found together)


################################################################################
# EDA
################################################################################
nrow(data_rf) #56
names(data_rf)
table(data_rf$Sites)
# Water quality
hist(data_rf$pH)
hist(data_rf$EC)
hist(data_rf$DO)
hist(data_rf$Temp)
hist(data_rf$Ammonia)
hist(data_rf$Phosphorous)
hist(data_rf$Nitrite)
hist(data_rf$Nitrate)
hist(data_rf$Total_Iron)
hist(data_rf$Phosphonate)
hist(data_rf$TDS)
hist(data_rf$Inorg_Nitrogen)
# Geographic
hist(data_rf$Elevation)
hist(data_rf$Flow)
hist(data_rf$Slope)
# Substrate
hist(data_rf$`Silt-Sand Substrate`)
hist(data_rf$`Gravel Substrate`)
hist(data_rf$`Cobble Substrate`)
hist(data_rf$`Boulder Substrate`)
hist(data_rf$`Bedrock Substrate`)
# Debris
hist(data_rf$`Woody Debris`)
hist(data_rf$`No Woody Debris`)
hist(data_rf$`Undercut Bank`)
hist(data_rf$`No Undercut Bank`)
# Macrophyte
hist(data_rf$`No Macrophytes`)
hist(data_rf$`Scarce Macrophytes`)
hist(data_rf$`Moderate Macrophytes`)
hist(data_rf$`Abundant Macrophytes`)
# Canopy

# River Depth

# River width

# Fish
hist(data_rf$`Heuningnes Redfin`)
hist(data_rf$`Cape Kurper`)
hist(data_rf$`Cape Galaxias`)
hist(data_rf$`Spotted Bass`)
hist(data_rf$`Bluegill Sunfish`)
hist(data_rf$`Common Carp`)

summary(data_rf$`Heuningnes Redfin`[data_rf$`Heuningnes Redfin`>0])
summary(data_rf$`Cape Kurper`[data_rf$`Cape Kurper`>0])
summary(data_rf$`Cape Galaxias`[data_rf$`Cape Galaxias`>0])
summary(data_rf$`Spotted Bass`[data_rf$`Spotted Bass`>0])
summary(data_rf$`Bluegill Sunfish`[data_rf$`Bluegill Sunfish`>0])
summary(data_rf$`Common Carp`)

hist(data_rf$`Heuningnes Redfin`[data_rf$`Heuningnes Redfin`>0])
hist(data_rf$`Cape Kurper`[data_rf$`Cape Kurper`>0])
hist(data_rf$`Cape Galaxias`[data_rf$`Cape Galaxias`>0])
hist(data_rf$`Spotted Bass`[data_rf$`Spotted Bass`>0])
hist(data_rf$`Bluegill Sunfish`[data_rf$`Bluegill Sunfish`>0])

s = 18

heun_dat <- data.frame(count = data_rf$`Heuningnes Redfin`[data_rf$`Heuningnes Redfin`>0])
heun_hist = ggplot(data = heun_dat, aes(x  = count)) + 
  geom_histogram(bins = 5, fill = "#006633", col = "#005522") + 
  xlab("Number of Individuals") + 
  ylab("Count") + 
  theme(title = element_text(size = s), text = element_text(size = s))
ggsave("heun_hist.png", plot = heun_hist, width = 6, height = 4)

kurp_dat <- data.frame(count = data_rf$`Cape Kurper`[data_rf$`Cape Kurper`>0])
kurp_hist = ggplot(data = kurp_dat, aes(x  = count)) + 
  geom_histogram(bins = 5, fill = "#006633", col = "#005522") + 
  xlab("Number of Individuals") + 
  ylab("Count") + 
  theme(title = element_text(size = s), text = element_text(size = s))
ggsave("kurp_hist.png", plot = kurp_hist, width = 6, height = 4)

gala_dat <- data.frame(count = data_rf$`Cape Galaxias`[data_rf$`Cape Galaxias`>0])
gala_hist = ggplot(data = gala_dat, aes(x  = count)) + 
  geom_histogram(bins = 5, fill = "#006633", col = "#005522") + 
  xlab("Number of Individuals") + 
  ylab("Count") + 
  theme(title = element_text(size = s), text = element_text(size = s))
ggsave("gala_hist.png", plot = gala_hist, width = 6, height = 4)

bass_dat <- data.frame(count = data_rf$`Spotted Bass`[data_rf$`Spotted Bass`>0])
bass_hist = ggplot(data = bass_dat, aes(x  = count)) + 
  geom_histogram(bins = 5, fill = "#006633", col = "#005522") + 
  xlab("Number of Individuals") + 
  ylab("Count") + 
  theme(title = element_text(size = s), text = element_text(size = s))
ggsave("bass_hist.png", plot = bass_hist, width = 6, height = 4)

blue_dat <- data.frame(count = data_rf$`Bluegill Sunfish`[data_rf$`Bluegill Sunfish`>0])
blue_hist = ggplot(data = blue_dat, aes(x  = count)) + 
  geom_histogram(bins = 5, fill = "#006633", col = "#005522") + 
  xlab("Number of Individuals") + 
  ylab("Count") + 
  theme(title = element_text(size = s), text = element_text(size = s))
ggsave("blue_hist.png", plot = blue_hist, width = 6, height = 4)
# data_rf$`No Woody Debris`

ranges_rf <- as.data.frame(matrix(NA, nrow = ncol(data_rf), ncol = 2))
row.names(ranges_rf) <- colnames(data_rf)


for(col in 1:ncol(data_rf)){
  ranges_rf[col,] <- round(range(data_rf[,col], na.rm = TRUE), 2)
}

sum(data_rf$`Heuningnes Redfin`>0)
sum(data_rf$`Cape Kurper`>0)
sum(data_rf$`Cape Galaxias`>0)
sum(data_rf$`Spotted Bass`>0)
sum(data_rf$`Bluegill Sunfish`>0)
sum(data_rf$`Common Carp`>0)
sum(data_rf$`Native`>0)
sum(data_rf$`Non-Native`>0)


sum(is.na(data_rf))

# mvabund
library(mvabund)

data(Tasmania) 
attach(Tasmania) 
tasmvabund <- mvabund(copepods) 
plot(tasmvabund ~ treatment, col = as.numeric(block))

tas.nb <- manyglm(copepods ~ block*treatment, family = "negative.binomial")
predict(tas.nb, type = "response")

plot(tas.nb)

anova(tas.nb, p.uni = "adjusted")
citation("mvabund")

?manyglm
