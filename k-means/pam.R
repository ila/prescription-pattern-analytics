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
library(Rtsne)      # for t-SNE plot
library(klaR)       # for k-modes

path = "C:\\Users\\WIN_KNIME_ATB\\Desktop\\dbcampania\\k-means\\"
data <- read.csv(paste(path, "dataset.csv", sep=""))
print("Data loaded and read successfully. ")

data$userid <- as.factor(data$userid)
data$co_codifa <- as.factor(data$co_codifa)

# works with up to 19k rows
test <- data[which(data$year == 2017 & (data$month > 10 | data$month < 3) & data$count > 1),]

# distance <- get_dist(data)
# fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# removing IDs before clustering
gower_dist <- daisy(test[, -1], metric="gower")
# print(summary(gower_dist))
print("Gower distance computed. ")

gower_mat <- as.matrix(gower_dist)
print("Gower matrix computed. ")
# print(test[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ])
# print(test[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ])

# k <- kmeans(data, centers = 2, nstart = 25)
# str(k)

# choosing the number of clusters
sil_width <- c(NA)

for(i in 6:12){
  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)
  print(paste("PAM computed for k = ", i, sep=""))
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# plot sihouette width (higher is better)
plot(1:12, sil_width, xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:12, sil_width)

optimal_k = max(sil_width[which(!is.na(sil_width))])
print(paste("Optimal score for k = ", optimal_k, sep=""))

pam_fit <- pam(gower_dist, diss = TRUE, k = which(sil_width == optimal_k))

pam_results <- test %>% 
  dplyr::select(-userid) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

# printing results and medoids
pam_results$the_summary
test[pam_fit$medoids, ]


