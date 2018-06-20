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

buffer = st_buffer(osgb_sites, 500)

ac_buffer = osgb_stat19[buffer, ]

osgb_joined = st_join(ac_buffer, buffer["Geographic Address"])

Crash_Counts = as.data.frame(table(osgb_joined$`Geographic Address`))
colnames(Crash_Counts) = c("Geographic Address", "Crash_Count_2016_500m")
Crash_Counts = Crash_Counts[order(-Crash_Counts$Crash_Count_2016_500m, decreasing = FALSE),]
head(Crash_Counts, n=20)
osgb_sites = left_join(osgb_sites, Crash_Counts, by = "Geographic Address")


# Buffer MIDAS Points at 100m

buffer = st_buffer(osgb_sites, 100)

ac_buffer = osgb_stat19[buffer, ]

osgb_joined = st_join(ac_buffer, buffer["Geographic Address"])

Crash_Counts = as.data.frame(table(osgb_joined$`Geographic Address`))
colnames(Crash_Counts) = c("Geographic Address", "Crash_Count_2016_100m")
Crash_Counts = Crash_Counts[order(-Crash_Counts$Crash_Count_2016_100m, decreasing = FALSE),]
head(Crash_Counts, n=20)
osgb_sites = left_join(osgb_sites, Crash_Counts, by = "Geographic Address")


# Look at Density Graph

library(spatstat)
library(sp)

#ac_sp = as(osgb_joined, "Spatial")
#sSp <- as(sp::SpatialPoints(ac_sp), "ppp")  # convert points to pp class
o = owin(xrange = range(osgb_joined$Location_Easting_OSGR), range(osgb_joined$Location_Northing_OSGR))
ac_pp = spatstat::ppp(x = osgb_joined$Location_Easting_OSGR, y = osgb_joined$Location_Northing_OSGR, window = o)
Dens <- density(ac_pp, adjust = 0.02)  # create density object
class(Dens)  # just for interest: it's got it's of pixel image class
plot(Dens)




plot(buffer$geometry)


# View Buffers in Mapview

mapview::mapview(buffer[, ])



