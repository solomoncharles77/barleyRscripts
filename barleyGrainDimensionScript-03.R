# Load libraries ----------------------------------------------------------

library(tidyverse)
library(DescTools)
library(FSA)
# Set path to project folder and subfolder --------------------------------

home <- getwd()
rawData <- file.path(home, file = "rawData")
rData <- file.path(home, file = 'rData')
tidyData <- file.path(home, file = 'tidyData')

# Read in and tidy data------------------------------------------------------------

grainLength <- read.csv(file.path(rawData, file = "Grain_Length_(mm).csv"))
grainWidth <- read.csv(file.path(rawData, file = "Grain_Lateral_Width_(mm).csv"))
grainDepth <- read.csv(file.path(rawData, file = "Grain_Dorsiventral_Width_(mm).csv"))
head(grainLength)
head(grainDepth)
head(grainWidth)

grainLength <- select(grainLength, -X) # Drop unwanted column
grainLength <- gather(grainLength, barleyLine, grainLength) # Convert to long format
grainLength$rn <- rownames(grainLength) # Reset rownames by adding an extra column called 'rn'

grainDepth <- rename(grainDepth, Over.Expr = Over.Exp) # Rename for consistency
grainDepth <- gather(grainDepth, barleyLine, grainDepth) # Convert to long format
grainDepth$rn <- rownames(grainDepth) # Reset rownames

grainWidth <- rename(grainWidth, Over.Expr = Over.Exp)
grainWidth <- gather(grainWidth, barleyLine, grainWidth)
grainWidth$rn <- rownames(grainWidth)

gDimension <- left_join(grainDepth, grainWidth, by = c('rn', 'barleyLine')) # Concatenate two data frames
head(gDimension)
gDimension$rn <- rownames(gDimension) #r Reset rownames again
grainDimension <- left_join(gDimension, grainLength, by = c('rn', 'barleyLine')) # Concatenate two data frames

grainDimension <- grainDimension %>% 
  select(-rn) %>%   # Drop unwanted column used for rowname reseting
  mutate(barleyLine = as.factor(barleyLine)) %>% 
  as.tibble()
head(grainDimension)
rm(gDimension, grainDepth, grainWidth, grainLength)

saveRDS(grainDimension, file.path(rData, file = 'barleyMatureGrainDimension'))
write.csv(grainDimension, file.path(tidyData, file = 'barleyMatureGrainDimension_tidy.csv'))

# Data Visualisation --------------------------------------------------------

grainDimension %>% 
  ggplot(aes(x=barleyLine, y=grainLength, fill=barleyLine)) +
  geom_boxplot()

grainDimension %>% 
  ggplot(aes(x=barleyLine, y=grainWidth, fill=barleyLine)) +
  geom_boxplot()

grainDimension %>% 
  ggplot(aes(x=barleyLine, y=grainDepth, fill=barleyLine)) +
  geom_boxplot()


# Data Description ----------------------------------------------

grainLengthDesc <- grainDimension %>% 
  select(barleyLine, grainLength) %>% 
  group_by_at(vars(barleyLine)) %>% 
  mutate(row_id=1:n()) %>% ungroup() %>% 
  spread(key=barleyLine, value=grainLength) %>% 
  select(-row_id)
Desc(grainLengthDesc)

grainWidthDesc <- grainDimension %>% 
  select(barleyLine, grainWidth) %>% 
  group_by_at(vars(barleyLine)) %>% 
  mutate(row_id=1:n()) %>% ungroup() %>% 
  spread(key=barleyLine, value=grainWidth) %>% 
  select(-row_id)
Desc(grainWidthDesc)

grainDepthDesc <- grainDimension %>% 
  select(barleyLine, grainDepth) %>% 
  group_by_at(vars(barleyLine)) %>% 
  mutate(row_id=1:n()) %>% ungroup() %>% 
  spread(key=barleyLine, value=grainDepth) %>% 
  select(-row_id)
Desc(grainDepthDesc)


# Stats tests -----------------------------------------------------

# From data description we know some of the variable for each grain attribute are normally distributed. 
# So we use nonparametric kruskal wallis test to test for statistical difference.

(grainLengthKW <- kruskal.test(grainLength ~ barleyLine, data = grainDimension))
(awnDataDT <- dunnTest(grainLength ~ barleyLine, data = grainDimension, method = 'bh'))

(grainWidthKW <- kruskal.test(grainWidth ~ barleyLine, data = grainDimension))
(grainWidthDT <- dunnTest(grainWidth ~ barleyLine, data = grainDimension, method = 'bh'))

(grainDepthKW <- kruskal.test(grainDepth ~ barleyLine, data = grainDimension))
(grainDepthDT <- dunnTest(grainDepth ~ barleyLine, data = grainDimension, method = 'bh'))



