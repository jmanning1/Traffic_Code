library(oneminutetrafficdata)
library(ukboundaries)
library(stplanr)
library(sf)
library(stringr)

# Load Geometries

geoms = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/","Gazetteer_All_Mposts_only.csv"))
head(geoms)

# Extract Relevant Columns and Rename

geom_slim = geoms[, c("BD", "DD", "Easting", "Northing")]
colnames(geom_slim) = c("Post", "Road", "Easting", "Northing")
head(geom_slim)

# Create GeographicAddress
a = substring(geom_slim$Post, 2)
a = gsub('/','', a)
a = as.data.frame(a)
geom_slim["Post_adj"] = a
geom_slim["Geographic Address"] = paste(geom_slim$Road, '/', geom_slim$Post_adj ,sep="")
geom_ready = geom_slim[c("Geographic Address", "Easting", "Northing")]
head(geom_ready)

write.csv(geom_ready, file = "D:/Documents/5872M-Dissertation/Data/Geometries/All_Traffic_Count_Locations.csv",row.names=FALSE)
