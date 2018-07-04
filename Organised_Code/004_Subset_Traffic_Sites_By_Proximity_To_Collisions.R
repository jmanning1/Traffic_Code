#Identify Traffic Sensor Sites within 500m of an collision

# Inputs:
# Site_Locations.csv
# Formatted Stat19 Data

# library(oneminutetrafficdata)
# library(ukboundaries)
# library(stplanr)
# library(sf)
# library(stringr)
# library(compare)
# library(dplyr)
# library(mapview)

# Load Cleaned Sites and Stat19 Data

sites = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Site_Locations.csv"))

stat19 = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Stat19_2016_2km_Subset.csv"))

# Make Spatial Geometry

ac_sites = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

osgb_stat19 = st_as_sf(stat19, coords = c("X", "Y"), crs = 27700)

# Transform to British National Grid

osgb_sites = st_transform(ac_sites, crs = 27700)

# Buffer MIDAS Points at 500m

buffer = st_buffer(osgb_sites, 500)

ac_buffer = osgb_stat19[buffer, ]

osgb_joined = st_join(ac_buffer, buffer["Geographic Address"])

# Identify number of crashs in locations

Crash_Counts = as.data.frame(table(osgb_joined$`Geographic Address`))
colnames(Crash_Counts) = c("Geographic Address", "Crash_Count_2016_500m")
Crash_Counts = Crash_Counts[order(-Crash_Counts$Crash_Count_2016_500m, decreasing = FALSE),]
head(Crash_Counts, n=20)
osgb_sites = left_join(osgb_sites, Crash_Counts, by = "Geographic Address")

table(Crash_Counts$Crash_Count_2016_500m)
