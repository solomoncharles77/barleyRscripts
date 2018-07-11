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

weightData <- read.csv(file.path(rawData, file = "barleyLines-grain-weight.csv"))
weightData <- as.tibble(weightData)
head(weightData)

days <- c(0, 5, 10, 15, 20, 25, 30, 35) # Here and in the following lines, I assign vectors and variables that will be populated in the for loop
wt <- c()
wtSD <- c()
oe <- c()
oeSD <- c()
gk <- c()
gkSD <- c()
DPA <- as.character(c("PrAsOv", "DPA5", "DPA10", "DPA15", "DPA20", "DPA25", "DPA30", "DPA35"))
counts <- c(1, 9, 17)
count2 <- c(1)

for(i in DPA){
  daa <- as.tibble(select(weightData, counts))     # Make a tibble of wt, oe and gk grain weights on corresponding days in DPA.
  assign(paste(i), daa)
  apply(daa, 2, hist, main = i, xlab = i )     # Plot histogram of each variable for description
  grainM <- apply(daa, 2, mean, na.rm = T)      # Calcute mean grain weight of barley lines
  wt[count2] <- grainM[[1]]
  oe[count2] <- grainM[[2]]    # extract mean weight for each barley line and add its respective vector of mean weights
  gk[count2] <- grainM[[3]]
  grainSD <- apply(daa, 2, sd, na.rm = T)   # Calculate standard deviation for grain weight in each barley line.
  wtSD[count2] <- grainSD[[1]]
  oeSD[count2] <- grainSD[[2]]    # Add relevant SD to their respective vectors
  gkSD[count2] <- grainSD[[3]]
  longDPA <- gather(daa, barleyLine, weight)
  longDPA <- mutate(longDPA, barleyLine = as.factor(barleyLine))   # Some stat analysis on grain weight.
  KW <- kruskal.test(weight ~ barleyLine, data = longDPA)
  assign(paste(i, "KW", sep = ""), KW)
  DT <- dunnTest(weight ~ barleyLine, data = longDPA, method = 'bh')
  assign(paste(i, "DT", sep = ""), DT)
  rm(daa, KW, DT, longDPA, grainM, grainSD,  i)
  counts <- counts + 1 
  count2 <- count2 + 1  
}                          

barleyLinesDevGrainWt <- as.tibble(cbind(days, wt, oe, gk))      # In the following lines, mean and sd data is manipulated to long format
ff <- gather(data = barleyLinesDevGrainWt, key = barleyLines, value = "values", wt, oe, gk)  # and combined.
ff$rn <- rownames(ff) # Reset rownames

barleyLinesDevGrainWtSD <- as.tibble(cbind(wtSD, oeSD, gkSD))
barleyLinesDevGrainWtSD <- rename(barleyLinesDevGrainWtSD, wt = wtSD, oe = oeSD, gk = gkSD )
lll <- gather(barleyLinesDevGrainWtSD, key = barleyLines, value = "sd")
lll$rn <- rownames(lll) # Reset rownames

grainMeanWeight_SD <- left_join(lll, ff, by = c("rn", "barleyLines"))
grainMeanWeight_SD <- select(grainMeanWeight_SD, -rn)

rm(lll, ff, count2, counts)  # Delete intermediate dataframes no longer needed.

saveRDS(grainMeanWeight_SD, file.path(rData, file = 'barleyGrainMeanWeight&SD'))
write.csv(grainMeanWeight_SD, file.path(tidyData, file = 'barleyGrainMeanWeight&SD.csv')) 


# Data Visualisation ------------------------------------------------------


ggplot(data = grainMeanWeight_SD, aes(x = days, y = values, color = barleyLines)) +
  geom_point() +
  geom_line()


