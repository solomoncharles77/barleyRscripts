# Load libraries ----------------------------------------------------------

library(tidyverse)
library(DescTools)
library(FSA)
# Set paths to project folder and subfolders --------------------------------

home <- getwd()
rawData <- file.path(home, file = "rawData")
rData <- file.path(home, file = 'rData')
tidyData <- file.path(home, file = 'tidyData')

# Read in and tidy data------------------------------------------------------------
rachisLength <- read.csv(file.path(rawData, file = "Rachis_Length_(cm).csv"))
tillerNo <- read.csv(file.path(rawData, file = "No_of_Tillers.csv"))
leafNo <- read.csv(file.path(rawData, file = "No_of_Internodes.csv"))
plantHeight <- read.csv(file.path(rawData, file = "Main_Stem_Height_(cm).csv"))
head(rachisLength)
head(tillerNo)
head(leafNo)
head(plantHeight)

rachisLength <- gather(rachisLength, barleyLine, rachisLength) # Convert to long format
rachisLength$rn <- rownames(rachisLength) # Reset rownames

tillerNo <- gather(tillerNo, barleyLine, tillerNo) # Convert to long format
tillerNo$rn <- rownames(tillerNo) # Reset rownames

leafNo <- gather(leafNo, barleyLine, leafNo) # Convert to long format
leafNo$rn <- rownames(leafNo) # Reset rownames

plantHeight <- gather(plantHeight, barleyLine, plantHeight) # Convert to long format
plantHeight$rn <- rownames(plantHeight) # Reset rownames

vGrowthData <- left_join(rachisLength, tillerNo, by = c('rn', 'barleyLine')) # Concatenate two data frames
head(vGrowthData)
vGrowthData$rn <- rownames(vGrowthData) #r Reset rownames again
vGrowthData <- left_join(vGrowthData, leafNo, by = c('rn', 'barleyLine')) # Concatenate two data frames

vGrowthData$rn <- rownames(vGrowthData) 
vGrowthData <- left_join(vGrowthData, plantHeight, by = c('rn', 'barleyLine')) 

vegGrowthData <- vGrowthData %>% 
  select(-rn) %>%   # Drop unwanted column used for rowname reseting
  mutate(barleyLine = as.factor(barleyLine)) %>% 
  as.tibble()
head(vegGrowthData)
rm(vGrowthData, tillerNo, leafNo, rachisLength, plantHeight)

saveRDS(vegGrowthData, file.path(rData, file = 'barleyVegGrowthData'))
write.csv(vegGrowthData, file.path(tidyData, file = 'barleyVegGrowthData-tidy.csv'))


# Visualisation -----------------------------------------------------------

vegGrowthData %>% 
  ggplot(aes(x=barleyLine, y=rachisLength, fill=barleyLine)) +
  geom_boxplot()

vegGrowthData %>% 
  ggplot(aes(x=barleyLine, y=tillerNo, fill=barleyLine)) +
  geom_boxplot()

vegGrowthData %>% 
  ggplot(aes(x=barleyLine, y=leafNo, fill=barleyLine)) +
  geom_boxplot()

leafNo <- tapply(vegGrowthData$leafNo, vegGrowthData$barleyLine, mean, na.rm = T)

leafNolong <- data.frame(as.list(leafNo)) %>% 
  gather(key = barleyLine, value = leafNo)

ggplot(leafNolong, aes(x=barleyLine, y=leafNo)) +
  geom_bar(stat="identity")

vegGrowthData %>% 
  ggplot(aes(x=barleyLine, y=plantHeight, fill=barleyLine)) +
  geom_boxplot()


# Data Description --------------------------------------------------------

# rachisLengthDesc <- vegGrowthData %>% 
#   select(barleyLine, rachisLength) %>% 
#   group_by_at(vars(barleyLine)) %>% 
#   mutate(row_id=1:n()) %>% ungroup() %>% 
#   spread(key=barleyLine, value=rachisLength) %>% 
#   select(-row_id)
# Desc(rachisLengthDesc)
# 
# tillerNoDesc <- vegGrowthData %>% 
#   select(barleyLine, tillerNo) %>% 
#   group_by_at(vars(barleyLine)) %>% 
#   mutate(row_id=1:n()) %>% ungroup() %>% 
#   spread(key=barleyLine, value=tillerNo) %>% 
#   select(-row_id)
# Desc(tillerNoDesc)
# 
# leafNoDesc <- vegGrowthData%>% 
#   select(barleyLine, leafNo) %>% 
#   group_by_at(vars(barleyLine)) %>% 
#   mutate(row_id=1:n()) %>% ungroup() %>% 
#   spread(key=barleyLine, value=leafNo) %>% 
#   select(-row_id)
# Desc(leafNoDesc)
# 
# plantHeightDesc <- vegGrowthData %>% 
#   select(barleyLine, plantHeight) %>% 
#   group_by_at(vars(barleyLine)) %>% 
#   mutate(row_id=1:n()) %>% ungroup() %>% 
#   spread(key=barleyLine, value=plantHeight) %>% 
#   select(-row_id) 
# Desc(plantHeightDesc)

# Stat test ---------------------------------------------------------------

# From data description we know some of the variable for each grain attribute are normally distributed. 
# So we use nonparametric kruskal wallis test to test for statistical difference.

(rachisLengthKW <- kruskal.test(rachisLength ~ barleyLine, data = vegGrowthData))
(rachisLengthDT <- dunnTest(rachisLength ~ barleyLine, data = vegGrowthData, method = 'bh'))

(tillerNoKW <- kruskal.test(tillerNo ~ barleyLine, data = vegGrowthData))
(tillerNoDT <- dunnTest(tillerNo ~ barleyLine, data = vegGrowthData, method = 'bh'))

(leafNoKW <- kruskal.test(leafNo ~ barleyLine, data = vegGrowthData))
(leafNoDT <- dunnTest(leafNo ~ barleyLine, data = vegGrowthData, method = 'bh'))

(plantHeightKW <- kruskal.test(plantHeight ~ barleyLine, data = vegGrowthData))
(plantHeightDT <- dunnTest(plantHeight ~ barleyLine, data = vegGrowthData, method = 'bh'))

