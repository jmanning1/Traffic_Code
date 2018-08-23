# Create Subset of Stat19 data within 2km of any point

library(oneminutetrafficdata)
library(ukboundaries)
library(stplanr)
library(sf)
library(stringr)
library(compare)
library(dplyr)
library(mapview)

# Load Cleaned Sites Data

sites = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Site_Locations_Cleaned.csv"))

# Make Spatial Geometry

ac_sites = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to British National Grid

osgb_sites = st_transform(ac_sites, crs = 27700)

# Run Stat19_2016.R first

osgb_stat19 = st_transform(ac_sf, crs = 27700)

plot(osgb_sites$geometry)

# Buffer MIDAS Points at 500m

buffer = st_buffer(osgb_sites, 2000)

ac_buffer = osgb_stat19[buffer, ]

coor = st_coordinates(ac_buffer$geometry)

output = cbind(ac_buffer, coor)

output$geometry = NULL

# Print Csv for 2km Limit from any traffic point.

write.csv(output, file = "D:/Documents/5872M-Dissertation/Data/Geometries/Stat19_2016_2km_Subset.csv",row.names=FALSE)

