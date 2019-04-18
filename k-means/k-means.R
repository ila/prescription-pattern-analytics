# trajectories analysis for co-prescription activities
# antibiotics as profiling tools, with additional layers (ways to use most prescribed products)
# factorial analysis to understand whether a GP makes constant prescriptions

# GPs, antibiotics, patient, date, patient age
# related to 8 antibiotics
# 5 clusters, 100 epochs

path = "C:\\Users\\WIN_KNIME_ATB\\Desktop\\dbcampania\\k-means\\"
data <- read.csv(paste(path, "dataset.csv", sep=""))
print("Data loaded and read successfully. ")

k <- kmeans(data, centers = 2, nstart = 25)