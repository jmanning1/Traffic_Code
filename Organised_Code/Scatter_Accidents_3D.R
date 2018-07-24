# Plot3d will crash RStudio, run in RGui

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

geometric
histCrash = table(cut(stat19$X, 50), cut(stat19$Y,50))
image2D(z = histCrash, border = "black")

stat19$Date = as.Date(stat19$Date, format = "%d/%m/%Y")
stat19$Date = as.numeric(stat19$Date)
stat19$Time = as.numeric(stat19$Time)

histCrash = table(cut(stat19$Time, 50), cut(stat19$Date,50))
hist3D(z = histCrash, border = "black")
image2D(z = histCrash, border = "black")
hist(stat19$Time,stat19$Date)


stat19$Date = as.Date(stat19$Date, format = "%d/%m/%Y")
hist(stat19$Date, breaks = 365)
hist(stat19$Date[stat19$Accident_Severity == 3], breaks = 365)

fatal = stat19[stat19$Accident_Severity == 1,]
severe = stat19[stat19$Accident_Severity == 2,]
slight = stat19[stat19$Accident_Severity == 3,]

nrow(stat19)

stat19$Time = as.numeric(as.POSIXct(stat19$Time, format = "%H:%M:%S")) %% 86400
stat19$Time = as.character(stat19$Time)
hist(stat19$Time, breaks = 1440)
t1 = qplot(stat19$Time, geom="histogram", binwidth = 3600, ylab = "Frequency", xlab = "Time", main = "Collision Times")
d1 = qplot(stat19$Date, geom="histogram", binwidth = 7, ylab = "Frequency", xlab = "Date", main = "Collision Dates")
w1 = qplot(stat19$Day_of_Week, geom="histogram", binwidth = 1, ylab = "Frequency", xlab = "Day of Week", main = "Collision Days")
t2 = qplot(fatal$Time, geom="histogram", binwidth = 3600, ylab = "Frequency", xlab = "Time", main = "Fatal Collision Times")
d2 = qplot(fatal$Date, geom="histogram", binwidth = 7, ylab = "Frequency", xlab = "Date", main = "Fatal  Collision Dates")
w2 = qplot(fatal$Day_of_Week, geom="histogram", binwidth = 1, ylab = "Frequency", xlab = "Day of Week", main = "Fatal Collision Days")
t3 = qplot(severe$Time, geom="histogram", binwidth = 3600, ylab = "Frequency", xlab = "Time", main = "Severe Collision Times")
d3 = qplot(severe$Date, geom="histogram", binwidth = 7, ylab = "Frequency", xlab = "Date", main = "Severe Collision Dates")
w3 = qplot(severe$Day_of_Week, geom="histogram", binwidth = 1, ylab = "Frequency", xlab = "Day of Week", main = "Severe Collision Days")
t4 = qplot(slight$Time, geom="histogram", binwidth = 3600, ylab = "Frequency", xlab = "Time", main = "Slight Collision Times")
d4 = qplot(slight$Date, geom="histogram", binwidth = 7, ylab = "Frequency", xlab = "Date", main = "Slight Collision Dates")
w4 = qplot(slight$Day_of_Week, geom="histogram", binwidth = 1, ylab = "Frequency", xlab = "Day of Week", main = "Slight Collision Days")

grid.arrange(t1, t4, t3, t2, nrow = 1, ncol = 4, top = "Collision Times")
grid.arrange(d1, d4, d3, d2, nrow = 1, ncol = 4, top = "Collision Dates")
grid.arrange(w1, w4, w3, w2, nrow = 1, ncol = 4, top = "Collision Days")



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

X = stat19_sev2[,c(31,32)]

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





