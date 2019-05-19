# trajectories analysis for co-prescription activities
# antibiotics as profiling tools, with additional layers (ways to use most prescribed products)
# factorial analysis to understand whether a GP makes constant prescriptions

# GPs, antibiotics, patient, date, patient age
# related to 8 antibiotics
# approx. 5 clusters, 100 epochs

# nb gerarchico
# fattibilità stagione/tempo
# ward distanza euclidea al quadrato
# albero di classificazione

# tenere i centroidi, 5-6 cluster, capire cosa succede


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)  # for plotting
library(fpc)
library(NbClust)

path = "/Users/ila/Desktop/codes/cmr-internship/k-means/"
data2017 <- read.csv(paste(path, "dataset2017.csv", sep=""))
data2010 <- read.csv(paste(path, "dataset2010.csv", sep=""))


# data preparation
data2017$userid <- as.factor(data2017$userid)
data2010$userid <- as.factor(data2010$userid)

subset2017 <- scale(data2017[,2:9])
subset2010 <- scale(data2010[,2:9])

# subset2017 <- data2017[-1]
# subset2010 <- data2010[-1]


# clustering
set.seed(1234)

nc2017 <- NbClust(subset2017, min.nc=2, max.nc=20, method="centroid")
nc2010 <- NbClust(subset2010, min.nc=2, max.nc=20, method="centroid")
# 2 followed by 3-6 with 2:9
# 2 followed by 3 with 2, 3, 7
# 2-3 with 2, 7 / all features


w2017 <- fviz_nbclust(subset2017, kmeans, method = "wss")
s2017 <- fviz_nbclust(subset2017, kmeans, method = "silhouette")

w2010 <- fviz_nbclust(subset2010, kmeans, method = "wss")
s2010 <- fviz_nbclust(subset2010, kmeans, method = "silhouette")

png(filename=paste(path, "optimal-clusters.png", sep=""), width=3500, height=2000, res=300)
  print(do.call("grid.arrange", c(list(w2017, s2017, w2010, s2010), ncol=2)))
dev.off()


# approach with 2 clusters
clusters2017 <- kmeans(subset2017, centers=2, iter.max=100, nstart=25)
clusters2010 <- kmeans(subset2010, centers=2, iter.max=100, nstart=25)


# plotcluster(subset2010, clusters2010$cluster)
# clusplot(subset2010, clusters2010$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


# png(filename=paste(path, "test.png", sep=""), width=3500, height=2000, res=300)
  # print(barplot(table(nc$Best.nc[1,])))
# dev.off()


# values with 2:9
mean(data2010[clusters2010$cluster==1,]$prescriptions)
# 18298.27
mean(data2010[clusters2010$cluster==2,]$prescriptions)
# 9041.124

sd(data2010[clusters2010$cluster==1,]$prescriptions)
# 5245.485
sd(data2010[clusters2010$cluster==2,]$prescriptions)
# 5123.937

mean(data2010[clusters2017$cluster==1,]$prescriptions)
# 12963.48
mean(data2010[clusters2017$cluster==2,]$prescriptions)
# 12310.55

sd(data2010[clusters2017$cluster==1,]$prescriptions)
# 7417.134
sd(data2010[clusters2017$cluster==2,]$prescriptions)
# 6592.356

