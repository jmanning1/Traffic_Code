stat19 = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Stat19_2016_2km_Subset.csv")))
head(stat19)
str(stat19)

install.packages("plot3D")

library("scatterplot3d")
library("rgl")
library(plot3D)

stat19_sev2 = stat19[stat19$Accident_Severity == 2,]


plot3d(stat19$X, stat19$Y, stat19$Time, col = stat19$Accident_Severity, size = 8)


plot3d(stat19_sev2$X, stat19_sev2$Y, stat19_sev2$Time, size = 5)



scatterplot3d(stat19$X, stat19$Y, stat19$Time, color = stat19$Accident_Severity)

ggplot(data = stat19, aes(X, Y, Time))

histCrash = table(cut(stat19$X, 50), cut(stat19$Y,50))


image2D(z = histCrash, border = "black")

# K Means Clustering

X = stat19_sev2[,c(10,31,32)]
X$Time =as.numeric(X$Time)

set.seed(25)

# within cluster sum of squares

wcss = vector()

for (i in 1:20){
	wcss [i] = sum(kmeans(X, i)$withinss)
}
plot(1:20, wcss,type = "b", main = "Clusters of Collisions")

set.seed(35)
kmeans = kmeans(X, 13, iter.max = 300, nstart = 20)

library(cluster)
clusplot(X$X, X$Y, 
	kmeans$cluster,
	shade = TRUE,
	color = TRUE
)

stat19_sev2$cluster = kmeans$cluster

plot3d(stat19_sev2$X, stat19_sev2$Y, stat19_sev2$Time, size = 5, col = stat19_sev2$cluster)

scatterplot3d(stat19_sev2$X, stat19_sev2$Y, stat19_sev2$Time, color = stat19_sev2$cluster, type = "h")

# Geometric Clustering

X = stat19_sev2[,c(,31,32)]

set.seed(25)

# within cluster sum of squares

wcss = vector()

for (i in 1:20){
	wcss [i] = sum(kmeans(X, i)$withinss)
}
plot(1:20, wcss,type = "b", main = "Clusters of Collisions")

set.seed(35)
kmeans = kmeans(X, 7, iter.max = 300, nstart = 20)

library(cluster)
clusplot(X, 
	kmeans$cluster,
	shade = TRUE,
	color = TRUE,
	plotchar = FALSE
)
stat19_sev2$cluster = kmeans$cluster

plot(stat19_sev2$X, stat19_sev2$Y, col = stat19_sev2$cluster)


