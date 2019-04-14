# trajectories analysis for co-prescription activities
# antibiotics as profiling tools, with additional layers (ways to use most prescribed products)
# factorial analysis to understand whether a GP makes constant prescriptions

# GPs, antibiotics, patient, date, patient age
# related to 8 antibiotics
# 5 clusters, 100 epochs

# 300k rows

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

path = "/Users/ila/Desktop/codes/cmr-internship/k-means/"
data <- read.csv(paste(path, "dataset.csv", sep=""))

data$userid <- as.factor(data$userid)
data$co_codifa <- as.factor(data$co_codifa)

distance <- get_dist(data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))