
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

awnData <- read.csv(file.path(rawData, file = "Awn_Length_(cm).csv"))
awnData <- as.tibble(awnData)

head(awnData)
awnData <- awnData %>% 
  gather(key=barleyLine, value=awnLength) %>% 
  mutate(barleyLine = as.factor(barleyLine))

saveRDS(awnData, file.path(rData, file = 'barleyAwnData'))
write.csv(awnData, file.path(tidyData, file = 'barleyAwnData-tidy.csv'))

# Data Visualisation ------------------------------------------------------

awnData %>% 
  ggplot( aes( x=barleyLine, y=awnLength, fill=barleyLine ) ) +
  geom_boxplot()

# Data Description --------------------------------------------------------

awnDataDesc <- awnData %>% 
  group_by_at(vars(-awnLength)) %>% 
  mutate(row_id=1:n()) %>% ungroup() %>% 
  spread(key=barleyLine, value=awnLength) %>% 
  select(-row_id)## Interesting!; the lines of code needed to spread a data I just gathered.

Desc(awnDataDesc)

# Stats test --------------------------------------------------------------
#From data exploration we know GK Awn length is not normally distributed. So we use nonparametric kruskal wallis test.
(awnDataKW <- kruskal.test(awnLength ~ barleyLine, data = awnData))
(awnDataDT <- dunnTest(awnLength ~ barleyLine, data = awnData, method = 'bh'))


