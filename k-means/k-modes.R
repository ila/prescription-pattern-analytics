# GPs, antibiotics, patient, date, patient age
# related to 8 antibiotics
# 5 clusters, 100 epochs

# 300k rows

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(Rtsne)      # for t-SNE plot
library(klaR)       # for k-modes

path = "C:\\Users\\WIN_KNIME_ATB\\Desktop\\dbcampania\\k-means\\"
data <- read.csv(paste(path, "dataset.csv", sep=""))
print("Data loaded and read successfully. ")

data$userid <- as.factor(data$userid)
data$co_codifa <- as.factor(data$co_codifa)

# works with up to 19k rows
test <- data[which(data$year == 2017 & (data$month > 10 | data$month < 3) & data$count > 1),]

# k-modes approach
results <-kmodes(test[, -1], 3, iter.max = 10, weighted = FALSE)