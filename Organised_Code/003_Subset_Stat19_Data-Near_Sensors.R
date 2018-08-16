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

sites = readr::read_csv(file.path("Data/Geometries/", "Site_Locations.csv"))


# Make Spatial Geometry

ac_sites = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to British National Grid

osgb_sites = st_transform(ac_sites, crs = 27700)

sts2 = tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_sites) +
  tm_dots() + 
  tm_layout(title = "Reduced MIDAS Webtris Sites", main.title.size = 10)

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(sts1, vp=viewport(layout.pos.col = 1))
print(sts2, vp=viewport(layout.pos.col = 2))
nrow(osgb_sites)

# Buffer MIDAS Points at 2000m

buffer_sites = st_buffer(osgb_sites, 2000)


# Load Roads

roads_shape = st_read("Data/Geometries/Roads/Links_nmv5_WP104_INRIX.shp")

# Transform to British National Grid

osgb_roads = st_transform(roads_shape, crs = 27700)

rds = tm_shape(uk) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'blue') + 
  tm_layout(title = "NTIS Network")


# Interactive Map
# mapview::mapview(osgb_roads) + mapview::mapview(osgb_sites)



# Run 001_Extract_Stat19_2016.R first tp get ac_sf

osgb_stat19 = st_transform(ac_sf, crs = 27700)

# Only Process Motorway and A road Collisions

road_types = c("1", "2", "3")

osgb_stat19 = osgb_stat19[osgb_stat19$`1st_Road_Class` %in% road_types,]

# Buffer road by 100m

buffer_road = st_buffer(osgb_roads, 100)

osgb_stat19 = osgb_stat19[buffer_road, ]

# all on network

on_network_colls = osgb_stat19

ab = tm_shape(uk) +
  tm_polygons(alpha = 0.1) +
  tm_shape(on_network_colls) +
  tm_dots() + 
  tm_layout(title = "Reduced Collisions \n Within 100m of \n NTIS Network", main.title.size = 10)

# Record the points on the network not in site of Traffic Sites

lost = sapply(st_intersects(osgb_stat19, buffer_sites),function(x){length(x)==0})

outside_stat19 = osgb_stat19[lost,]

ao = tm_shape(uk) +
  tm_polygons(alpha = 0.1) +
  tm_shape(outside_stat19) +
  tm_dots() + 
  tm_layout(title = "On NTIS Network \n Farther than 2km from \n Webtris Sites", main.title.size = 10)

# Get near sites only

ac_buffer = osgb_stat19[buffer_sites, ]

ad = tm_shape(uk) +
  tm_polygons(alpha = 0.1) +
  tm_shape(ac_buffer) +
  tm_dots() + 
  tm_layout(title = "Reduced Collisions \n Within 2km of \n Webtris Sites", main.title.size = 10)

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,4)))
print(aa, vp=viewport(layout.pos.col = 1))
print(ab, vp=viewport(layout.pos.col = 2))
print(ad, vp=viewport(layout.pos.col = 3))
print(ao, vp=viewport(layout.pos.col = 4))
nrow(ac_buffer)

ae = tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(ac_buffer) +
  tm_dots() + 
  tm_layout(title = "Reduced Collisions by \n Geographical Location", main.title.size = 10)

# Lost on network only

outside_stat19 = outside_stat19[buffer_road, ]

ggplot(data = outside_stat19, aes(Location_Easting_OSGR, Location_Northing_OSGR)) +
  geom_point(colour = outside_stat19$Accident_Severity)

potential_high_collisions = table(cut(outside_stat19$Location_Easting_OSGR, 100), cut(outside_stat19$Location_Northing_OSGR,100))

image2D(z = potential_high_collisions, border = "black")

# Extract Co-ordinates from Geometry to Output into csv.

coor = st_coordinates(ac_buffer$geometry)

output = cbind(ac_buffer, coor)

output$geometry = NULL

colnames(output) = c("Accident_Index", "Location_Easting_OSGR", "Location_Northing_OSGR", "Police_Force", "Accident_Severity", "Number_of_Vehicles", "Number_of_Casualties", "Date", "Day_of_Week", "Time", "Local_Authority_(District)", "Local_Authority_(Highway)", "1st_Road_Class", "1st_Road_Number", "Road_Type", "Speed_limit", "Junction_Detail", "Junction_Control", "2nd_Road_Class", "2nd_Road_Number", "Pedestrian_Crossing-Human_Control", "Pedestrian_Crossing-Physical_Facilities", "Light_Conditions", "Weather_Conditions", "Road_Surface_Conditions", "Special_Conditions_at_Site", "Carriageway_Hazards", "Urban_or_Rural_Area", "Did_Police_Officer_Attend_Scene_of_Accident", "LSOA_of_Accident_Location", "X", "Y")



# Print csv for 2km Limit from any traffic point.

write.csv(output, file = "Data/Geometries/Stat19_2016_2km_Subset.csv",row.names=FALSE)


# Histograms

# Within 2km of Webtris Locations


t1 = ggplot(output, aes(TimeNum, fill = Accident_Severity)) +
  geom_histogram(bins = 24) 
t1 = ggplot(output$TimeNum[output$Accident_Severity == 2], aes(TimeNum, fill = Accident_Severity)) +
  geom_histogram(bins = 24) 

+
  scale_fill_manual(values = c("red", "yellow", "green"))

t1 = qplot(output$Time, bins = 24)
tsl = qplot(output$Time[output$Accident_Severity == 3], bins = 24) +
  scale_fill_manual(values = "green")
tse = qplot(output$Time[output$Accident_Severity == 2], bins = 24) +
  scale_fill_manual(values = "orange")
tfa = qplot(output$Time[output$Accident_Severity == 1], bins = 24) +
  scale_fill_manual(values = "red")

grid.arrange(t1,tsl,tse,tfa, nrow = 1, ncol = 4)

y1 = qplot(output$Date, bins = 366)
ysl = qplot(output$Date[output$Accident_Severity == 3], bins = 366)
yse = qplot(output$Date[output$Accident_Severity == 2], bins = 366)
yfa = qplot(output$Date[output$Accident_Severity == 1], bins = 366)
y2 = qplot(output$Date, bins = 52)
ysl2 = qplot(output$Date[output$Accident_Severity == 3], bins = 52)
yse2 = qplot(output$Date[output$Accident_Severity == 2], bins = 52)
yfa2 = qplot(output$Date[output$Accident_Severity == 1], bins = 52)

grid.arrange(y1,ysl,yse,yfa,y2,ysl2,yse2,yfa2, nrow = 2, ncol = 4)

d1 = qplot(output$Day_of_Week, bins = 7)
dsl = qplot(output$Day_of_Week[output$Accident_Severity == 3], bins = 7)
dse = qplot(output$Day_of_Week[output$Accident_Severity == 2], bins = 7)
dfa = qplot(output$Day_of_Week[output$Accident_Severity == 1], bins = 7)

grid.arrange(d1,dsl,dse,dfa, nrow = 1, ncol = 4)
