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

barleyLinesFertility1 <- read.csv(file.path(rawData, file = "barley-lines-fertility-raw.csv"))
barleyLinesFertility2 <- read.csv(file.path(rawData, file = "barley_lines-fertility-data2.csv"))

barleyLinesFertility1
barleyLinesFertility2

# Data Manipulation and Visualisation -------------------------------------

# I collected data for plant fertility twice. Both show identical trends. I therefore combined both data

combinedFert <- rbind(barleyLinesFertility1, barleyLinesFertility2)


perSpikeFloretNo <- combinedFert %>%  # Compare spike no in the barley lines.
  select(1, 4, 7) %>% 
  rename(WT=1, OE=2, GK=3) %>% 
  gather(key = barleyLines, value = floretNo) %>% 
  mutate(barleyLines = as.factor(barleyLines)) %>% 
  group_by(barleyLines) 

perSpikeFloretNo %>% 
  ggplot(aes(x=barleyLines, y=floretNo, fill = barleyLines)) +
  geom_boxplot()

percentFertility <- combinedFert %>%    # Calculate percentage of filled spike as percentage fertility.
  mutate(WT = .[[2]] / .[[1]] * 100, OE = .[[5]] /.[[4]] * 100, GK = .[[8]] / .[[7]] *100) %>% 
  select(10, 11, 12) %>% 
  gather(key=barleyLines, value=spikePercentageFertility) %>% 
  mutate(barleyLines = as.factor(barleyLines)) %>% 
  group_by(barleyLines)

ggplot(percentFertility, aes(x=barleyLines, y=spikePercentageFertility, fill = barleyLines)) +
  geom_boxplot()


# Statistical Analysis ----------------------------------------------------

(floretNoKW <- kruskal.test(floretNo ~ barleyLines, data = perSpikeFloretNo))
(floretNoDT <- dunnTest(floretNo ~ barleyLines, data = perSpikeFloretNo, method = 'bh'))


(perFertKW <- kruskal.test(spikePercentageFertility ~ barleyLines, data = percentFertility))
(perFertDT <- dunnTest(spikePercentageFertility ~ barleyLines, data = percentFertility, method = 'bh'))


# Save data ---------------------------------------------------------------

saveRDS(combinedFert, file.path(rData, file = 'barleyFertilityData'))
write.csv(combinedFert, file.path(tidyData, file = 'barleyFertility_tidy.csv'))

saveRDS(perSpikeFloretNo, file.path(rData, file = 'barleyPerFloretNumber'))
write.csv(perSpikeFloretNo, file.path(tidyData, file = 'barleyPerFloretNumber_tidy.csv'))

saveRDS(percentFertility, file.path(rData, file = 'barleySpikePercentageFertility'))
write.csv(percentFertility, file.path(tidyData, file = 'barleyPercentageFertility_tidy.csv'))










# Reserve code for bar plot -----------------------------------------------

# spikeNo1bar <- combinedFert %>%
#   select(1,4,7) %>%
#   rename(WT=1, OE=2, GK=3) %>%
#   gather(key = barleyLines, value = floretNo)
# 
# spikeNo1barrr <- tapply(spikeNo1bar$floretNo, spikeNo1bar$barleyLines, mean, na.rm = T)
#   
# spikeNo1barrr
# 
# ff <- data.frame(as.list(spikeNo1barrr)) %>% 
#   gather(key = barleyLines, value = floretNo)
# 
# gp <- ggplot(ff, aes(x=barleyLines, y=floretNo)) +
#   geom_bar(stat="identity")
# 
# gpo
# 
