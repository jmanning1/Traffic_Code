# Extract Accidents Effectively - Create Subset of Stat19 data within 2km of any traffic point

# Inputs:
#   Site_Locations.csv
#   Links_nmv5_WP104_INRIX.shp - Network Road Locations

# Output:
#   Stat19_2016_2km_Subset.csv


# Librarys Used:
# library(oneminutetrafficdata)
# library(ukboundaries)
# library(stplanr)
# library(sf)
# library(stringr)
# library(compare)
# library(dplyr)
# library(mapview)
# library(rgdal)

# Load Cleaned Sites Data

sites = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Site_Locations.csv"))


# Make Spatial Geometry

ac_sites = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to British National Grid

osgb_sites = st_transform(ac_sites, crs = 27700)

# Buffer MIDAS Points at 2000m

buffer = st_buffer(osgb_sites, 2000)

# Load Roads

roads_shape = st_read("D:/Documents/5872M-Dissertation/Data/Geometries/Roads/Links_nmv5_WP104_INRIX.shp")

# Transform to British National Grid

osgb_roads = st_transform(roads_shape, crs = 27700)


# Run Stat19_2016.R first tp get ac_sf

osgb_stat19 = st_transform(ac_sf, crs = 27700)

# Only Process Motorway and A road Collisions

road_types = c("1", "2", "3")

osgb_stat19 = osgb_stat19[osgb_stat19$`1st_Road_Class` %in% road_types,]

ac_buffer = osgb_stat19[buffer, ]

# Buffer road by 200m

buffer_road = st_buffer(osgb_roads, 100)

new_buffer = ac_buffer[buffer_road, ]

# Extract Co-ordinates from Geometry to Output into csv.

coor = st_coordinates(new_buffer$geometry)

output = cbind(traf_stat19, coor)

output$geometry = NULL

# Print csv for 2km Limit from any traffic point.

write.csv(output, file = "D:/Documents/5872M-Dissertation/Data/Geometries/Stat19_2016_2km_Subset.csv",row.names=FALSE)