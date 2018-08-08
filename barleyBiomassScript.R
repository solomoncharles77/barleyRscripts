# Load libraries ----------------------------------------------------------

library(tidyverse)
library(magrittr)
library(DescTools)
library(FSA)
# Set path to project folder and subfolder --------------------------------

home <- getwd()
rawData <- file.path(home, file = "rawData")
rData <- file.path(home, file = 'rData')
tidyData <- file.path(home, file = 'tidyData')

# Read in and tidy data------------------------------------------------------------

mockFreshWholePlantData <- read.csv(file.path(rawData, file = "mock-barley-fresh-weight.csv"))
mockDryWholePlantData <- read.csv(file.path(rawData, file = "mock-barley-dry-weight.csv"))
freshBarleyPlantBiomass <- read.csv(file.path(rawData, file = "main-barley-fresh-weight.csv"))
dryBarleyPlantBiomass <- read.csv(file.path(rawData, file = "main-barley-dry-weight.csv"))

head(freshBarleyPlantBiomass)
head(dryBarleyPlantBiomass)
# Data Exploration --------------------------------------------------------

barleyBiomass <- as.tibble(cbind(freshBarleyPlantBiomass, dryBarleyPlantBiomass))

barleyBiomass %<>%                      # Calculate Root Shoot ratio
  mutate(WTRootShootRatio = WT.root/WT.stem) %>% 
  mutate(OERootShootRatio = OE.root/OE.stem) %>% 
  mutate(GKRootShootRatio = GK.root/GK.stem)

head(barleyBiomass)
  
barleyBiomass <- barleyBiomass[c(1, 4:6, 13, 2, 7:9, 14, 3, 10:12, 15)] #Reorder the variables


mass <- c("FreshBiomass", "DryBiomass", "ShootBiomass", "RootBiomass", "RSratio")
counter <- c(1, 6, 11)

for (v in mass){
  gm <- as.tibble(select(barleyBiomass, counter))
  gm <- rename(gm, WT=1, OE=2, GK=3)
  gm <- gather(gm, key = barleyLines, value = value)
  gm <- mutate(gm, barleyLines = as.factor(barleyLines))
  assign(paste(v), gm)
  KW <- kruskal.test(value ~ barleyLines, data = gm)
  assign(paste(v, "KW", sep = ""), KW)
  DT <- dunnTest(value ~ barleyLines, data = gm, method = 'bh')
  assign(paste(v, "DT", sep = ""), DT)
  gg <- ggplot(data=gm, aes(x=barleyLines, y = value, fill = barleyLines)) +
    geom_boxplot() 
  assign(paste(v, "plot", sep =""), gg)
  rm(gm, gg, KW, DT)
  counter <- counter + 1
}

FreshBiomassplot
DryBiomassplot
ShootBiomassplot
RootBiomassplot
RSratioplot

