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

subset2017 <- data2017[,2:9]
subset2010 <- data2010[,2:9]

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
n2017 <- grid.arrange(w2017, s2017, ncol=2, top="Year: 2017")

w2010 <- fviz_nbclust(subset2010, kmeans, method = "wss")
s2010 <- fviz_nbclust(subset2010, kmeans, method = "silhouette")
n2010 <- grid.arrange(w2010, s2010, ncol=2, top = "\nYear: 2010")

png(filename=paste(path, "optimal-clusters.png", sep=""), width=2000, height=1750, res=300)
  print(do.call("grid.arrange", c(list(n2017, n2010), nrow=2)))
dev.off()


# approach with 2 clusters
clusters2017 <- kmeans(subset2017, centers=4, iter.max=100, nstart=25)
clusters2010 <- kmeans(subset2010, centers=4, iter.max=100, nstart=25)

clusters2017_2 <- kmeans(subset2017, centers=2, iter.max=100, nstart=25)
clusters2010_2 <- kmeans(subset2010, centers=2, iter.max=100, nstart=25)

c2010 <- grid.arrange(fviz_cluster(clusters2010, data = subset2010), ncol=1, top = "\nYear: 2010")
c2017 <- grid.arrange(fviz_cluster(clusters2017, data = subset2017), ncol=1, top = "\nYear: 2017")

c2010_2 <- grid.arrange(fviz_cluster(clusters2010_2, data = subset2010), ncol=1, top = "\nYear: 2010")
c2017_2 <- grid.arrange(fviz_cluster(clusters2017_2, data = subset2017), ncol=1, top = "\nYear: 2017")

png(filename=paste(path, "clusters-2.png", sep=""), width=2500, height=2500, res=300)
  print(do.call("grid.arrange", c(list(c2010_2, c2017_2), nrow=2)))
dev.off()

png(filename=paste(path, "clusters-4.png", sep=""), width=2500, height=2500, res=300)
print(do.call("grid.arrange", c(list(c2010, c2017), nrow=2)))
dev.off()


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

mean(data2017[clusters2017$cluster==1,]$prescriptions)
# 12963.48
mean(data2017[clusters2017$cluster==2,]$prescriptions)
# 12310.55

sd(data2017[clusters2017$cluster==1,]$prescriptions)
# 7417.134
sd(data2017[clusters2017$cluster==2,]$prescriptions)
# 6592.356

