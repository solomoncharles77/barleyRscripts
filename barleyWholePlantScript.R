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
freshBarleyPlantWeight <- read.csv(file.path(rawData, file = "main-barley-fresh-weight.csv"))
dryBarleyPlantDryWeight <- read.csv(file.path(rawData, file = "main-barley-dry-weight.csv"))

# Data Exploration --------------------------------------------------------

wholePlantDryWeight <- dryBarleyPlantDryWeight %>% 
  select(1, 4, 7) %>% 
  rename(WT=WT.total, OE=OE.total, GK=GK.total) %>% 
  gather(key = barleyLines, value = PlantDryWeight)

wholePlantDryWeight %>% 
  ggplot(aes(x=barleyLines, y=PlantDryWeight, fill = barleyLines)) +
  geom_boxplot()


barleyPlantShootDryWeight <- dryBarleyPlantDryWeight %>% 
  select(2, 5, 8) %>% 
  rename(WT=WT.stem, OE=OE.stem, GK=GK.stem) %>% 
  gather(key = barleyLines, value = ShootDryWeight)

barleyPlantShootDryWeight %>% 
  ggplot(aes(x=barleyLines, y=ShootDryWeight, fill = barleyLines)) +
  geom_boxplot()


barleyPlantRootDryWeight <- dryBarleyPlantDryWeight %>% 
  select(3, 6, 9) %>% 
  rename(WT=WT.root, OE=OE.root, GK=GK.root) %>% 
  gather(key = barleyLines, value = RootDryWeight)

barleyPlantRootDryWeight %>% 
  ggplot(aes(x=barleyLines, y=RootDryWeight, fill = barleyLines)) +
  geom_boxplot()


dryBarleyPlantDryWeight %<>%
  mutate(WTRootShootRatio = WT.stem/WT.root) %>% 
  mutate(OERootShootRatio = OE.stem/OE.root) %>% 
  mutate(GKRootShootRatio = GK.stem/GK.root)  

barleyPlantRSratio <- dryBarleyPlantDryWeight %>% 
  select(WTRootShootRatio, OERootShootRatio, GKRootShootRatio) %>% 
  rename(WT=WTRootShootRatio, OE=OERootShootRatio, GK=GKRootShootRatio) %>% 
  gather(key = barleyLines, value = RSratio) 

barleyPlantRSratio %>% 
  ggplot(aes(x=barleyLines, y=RSratio, fill = barleyLines)) +
  geom_boxplot()
