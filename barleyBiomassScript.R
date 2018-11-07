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
  gm <- rename(gm, Wt=1, "zeo1.b" = 2, "gig1.a" = 3)
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


# Visualisation and plot customization -----------------------------------------

FreshBiomassplot
DryBiomassplot
ShootBiomassplot
RootBiomassplot
RSratioplot


# Save data ---------------------------------------------------------------

saveRDS(barleyBiomass, file.path(rData, file = 'barleyBiomassData'))
write.csv(barleyBiomass, file.path(tidyData, file = 'barleyBiomassData_tidy.csv'))

saveRDS(FreshBiomass, file.path(rData, file = 'barleyFreshBiomass'))
write.csv(FreshBiomass, file.path(tidyData, file = 'barleyFreshBiomass_tidy.csv'))

saveRDS(DryBiomass, file.path(rData, file = 'barleyDryBiomass'))
write.csv(DryBiomass, file.path(tidyData, file = 'barleyDryBiomass_tidy.csv'))

saveRDS(ShootBiomass, file.path(rData, file = 'barleyShootBiomass'))
write.csv(ShootBiomass, file.path(tidyData, file = 'barleyShootBiomass_tidy.csv'))

saveRDS(RootBiomass, file.path(rData, file = 'barleyRootBiomass'))
write.csv(RootBiomass, file.path(tidyData, file = 'barleyRootBiomass_tidy.csv'))

saveRDS(RSratio, file.path(rData, file = 'barleyRSratio'))
write.csv(RSratio, file.path(tidyData, file = 'barleyRSratio_tidy.csv'))





