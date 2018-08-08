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
barleyLinesFertility2

spikeNo1 <- barleyLinesFertility1 %>% 
  select(1, 4, 7) %>% 
  rename(WT=1, OE=2, GK=3) %>% 
  gather(key = barleyLines, value = floretNo) %>% 
  group_by(barleyLines) 

spikeNo1 %>% 
  ggplot(aes(x=barleyLines, y=floretNo, fill = barleyLines)) +
  geom_boxplot()

spikeNo1bar <- barleyLinesFertility1 %>% 
  select(1,4,7) %>% 
  rename(WT=1, OE=2, GK=3) %>% 
  gather(key = barleyLines, value = floretNo) 

spikeNo1barrr <- tapply(spikeNo1bar$floretNo, spikeNo1bar$barleyLines, mean)
  
spikeNo1barrr

ff <- data.frame(as.list(spikeNo1barrr)) %>% 
  gather(key = barleyLines, value = floretNo)

gp <- ggplot(ff, aes(x=barleyLines, y=floretNo)) +
  geom_bar(stat="identity")

gp

pff1 <- barleyLinesFertility1 %>% 
  mutate(WT = .[[2]] / .[[1]] * 100, OE = .[[5]] /.[[4]] * 100, GK = .[[8]] / .[[7]] *100) %>% 
  select(10, 11, 12) %>% 
  gather(key=barleyLines, value=spikepercentagefertility)

ggplot(pff1, aes(x=barleyLines, y=spikepercentagefertility)) +
  geom_boxplot()

pff2 <- barleyLinesFertility2 %>%                                                                # Percentage of filled florets
  mutate(WT = .[[2]] / .[[1]] * 100, OE = .[[5]] /.[[4]] * 100, GK = .[[8]] / .[[7]] *100) %>%   # Calculate percentage of filled grain for each line.
  select(10, 11, 12) %>% 
  gather(key=barleyLines, value=spikepercentagefertility)

ggplot(pff2, aes(x=barleyLines, y=spikepercentagefertility)) +
  geom_boxplot()
