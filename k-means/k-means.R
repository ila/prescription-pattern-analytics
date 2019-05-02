# trajectories analysis for co-prescription activities
# antibiotics as profiling tools, with additional layers (ways to use most prescribed products)
# factorial analysis to understand whether a GP makes constant prescriptions

# GPs, antibiotics, patient, date, patient age
# related to 8 antibiotics
# approx. 5 clusters, 100 epochs

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)  # for plotting
library(fpc)

path = "C:\\Users\\WIN_KNIME_ATB\\Desktop\\dbcampania\\k-means\\"
data2017 <- read.csv(paste(path, "dataset2017.csv", sep=""))
data2010 <- read.csv(paste(path, "dataset2010.csv", sep=""))


# data preparation
data2017$userid <- as.factor(data2017$userid)
data2010$userid <- as.factor(data2010$userid)

subset2017 <- scale(data2017[,2:9])
subset2010 <- scale(data2010[,2:9])


# clustering
set.seed(1234)

w2017 <- fviz_nbclust(subset2017, kmeans, method = "wss")
s2017 <- fviz_nbclust(subset2017, kmeans, method = "silhouette")

w2010 <- fviz_nbclust(subset2010, kmeans, method = "wss")
s2010 <- fviz_nbclust(subset2010, kmeans, method = "silhouette")

png(filename=paste(path, "optimal-clusters.png", sep=""), width=3500, height=2000, res=300)

print(do.call("grid.arrange", c(list(w2017, s2017, w2010, s2010), ncol=2)))

dev.off()


# approach with 6 clusters
clusters2017 <- kmeans(subset2017, centers=6, iter.max=100, nstart=25)
clusters2010 <- kmeans(subset2010, centers=6, iter.max=100, nstart=25)


# plotcluster(subset2010, clusters2010$cluster)
# clusplot(subset2010, clusters2010$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


