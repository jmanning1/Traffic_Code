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

# Create Spatial Collisions Map



raster_template = raster(extent(100000, 700000, 0, 750000), resolution = 10000,
                         crs = st_crs(27700)$proj4string)

col_raster1 = rasterize(on_network_colls, raster_template, 
                        field = 1, fun = "count")
# All on NTIS Network - Collision Density

all_km10 = tm_shape(col_raster1) +
  tm_raster(title = "", breaks = c(1,5,10,25,50,100,150,200,250,300)) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 100000, xmax = 700000, ymax = 750000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'blue', alpha = 0.1) +
  tm_layout(title = "All Collisions",
            main.title.size = 10,
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

col_raster2 = rasterize(ac_buffer, raster_template, 
                        field = 1, fun = "count")


within_2km_km10 = tm_shape(col_raster2) +
  tm_raster(title = "", breaks = c(1,5,10,25,50,100,150,200,250,300)) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 100000, xmax = 700000, ymax = 750000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'blue', alpha = 0.1) +
  tm_layout(title = "Collisions Near Sites", 
            main.title.size = 10,
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

col_raster3 = rasterize(outside_stat19, raster_template, 
                        field = 1, fun = "count")

Farther_2km_km10 = tm_shape(col_raster3) +
  tm_raster(title = "", breaks = c(1,5,10,25,50,100,150,200,250,300)) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 100000, xmax = 700000, ymax = 750000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'blue', alpha = 0.1)  +
  tm_layout(title = "Collisions Away From \n Sites", 
            main.title.size = 10,
            legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(all_km10, vp=viewport(layout.pos.col = 1))
print(within_2km_km10, vp=viewport(layout.pos.col = 2))
print(Farther_2km_km10, vp=viewport(layout.pos.col = 3))


