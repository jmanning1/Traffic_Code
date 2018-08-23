library(oneminutetrafficdata)
library(ukboundaries)
library(stplanr)
library(sf)
library(stringr)
library(compare)
library(dplyr)

geoms = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/","All_Traffic_Count_Locations.csv"))
data = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/csv_files/", "10010518.tcd.csv"))

names(data)
names(geoms)

joined = merge(geoms, data, by.x = "Geographic Address", by.y = "Geographic Address", all.y = TRUE)


nrow(joined)
nrow(data)
nrow(geoms)

head(joined)

plot(joined$Easting, joined$Northing)

table(data$'Geographic Address' == geoms$'Geographic Address')

a = as.data.frame(unique(data$`Geographic Address`))
b = as.data.frame(unique(geoms$`Geographic Address`))
comparison = compare(a, b, allowAll = TRUE)
comparison$tM
colnames(a) = "geog"
colnames(b) = "geog"
fail = anti_join(a,b)

write.csv(fail, file = "D:/Documents/5872M-Dissertation/Data/Geometries/Missing_Halogen_Locations.csv",row.names=FALSE)
