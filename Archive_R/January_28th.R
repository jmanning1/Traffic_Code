# Extract of Test sites from Halogen Data

library(oneminutetrafficdata)
library(sf)
library(dplyr)
library(data.table)
library(plyr)

stat19 = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Stat19_2016_2km_Subset.csv")))

colnames(stat19) = c("Accident_Index", "Location_Easting_OSGR", "Location_Northing_OSGR", "Police_Force", "Accident_Severity", "Number_of_Vehicles", "Number_of_Casualties", "Date", "Day_of_Week", "Time", "Local_Authority_(District)", "Local_Authority_(Highway)", "1st_Road_Class", "1st_Road_Number", "Road_Type", "Speed_limit", "Junction_Detail", "Junction_Control", "2nd_Road_Class", "2nd_Road_Number", "Pedestrian_Crossing-Human_Control", "Pedestrian_Crossing-Physical_Facilities", "Light_Conditions", "Weather_Conditions", "Road_Surface_Conditions", "Special_Conditions_at_Site", "Carriageway_Hazards", "Urban_or_Rural_Area", "Did_Police_Officer_Attend_Scene_of_Accident", "LSOA_of_Accident_Location", "X", "Y")


jan28 = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Halogen_Site_csv/", "2016-01-28.csv")))

colnames(jan28) = c("Control Office","Geographic Address",
"Year","Month",
"Day","Day of Week",
"Type of Day","Days After Nearest Bank Holiday",
"Time GMT","Number of Lanes",
"Flow Category 1","Flow Category 2",
"Flow Category 3","Flow Category 4",
"Average Speed Lane 1","Total Flow Lane 1",
"Occupancy Lane 1","Average Headway Lane 1",
"Average Speed Lane 2","Total Flow Lane 2",
"Occupancy Lane 2","Average Headway Lane 2",
"Average Speed Lane 3","Total Flow Lane 3",
"Occupancy Lane 3","Average Headway Lane 3",
"Average Speed Lane 4","Total Flow Lane 4",
"Occupancy Lane 4","Average Headway Lane 4",
"Average Speed Lane 5","Total Flow Lane 5",
"Occupancy Lane 5","Average Headway Lane 5",
"Average Speed Lane 6","Total Flow Lane 6",
"Occupancy Lane 6","Average Headway Lane 6")

stat19_jan28 = stat19[stat19$Date == "28/01/2016",]
str(stat19_jan28)
stat19_spatial = st_as_sf(stat19_jan28, coords = c("X", "Y"), crs = 27700)

nrow(stat19_spatial)

# Extract only the nearby Traffic Nodes

sites = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Site_Locations_Cleaned.csv"))

ac_sites = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

osgb_sites = st_transform(ac_sites, crs = 27700)

buffer = st_buffer(osgb_sites, 500)

ac_buffer = stat19_spatial[buffer, ]

osgb_joined = st_join(ac_buffer, buffer["Geographic Address"])

Crash_Counts = as.data.frame(table(osgb_joined$`Geographic Address`))
colnames(Crash_Counts) = c("Geographic Address", "Crash_Count_2016_500m")

jan28_slim = jan28[jan28$`Geographic Address` %in% Crash_Counts$`Geographic Address`,]

coor = st_coordinates(osgb_sites$geometry)

coor = as.data.frame(cbind(osgb_sites$`Geographic Address`, coor))
coor$X = as.numeric(as.character(coor$X))
coor$Y =  as.numeric(as.character(coor$Y))
colnames(coor) = c("Geographic Address","X","Y")

jan28_joined = left_join(jan28_slim, coor, by = "Geographic Address")
jan28_joined = as.data.frame(jan28_joined)


jan28_spatial = st_as_sf(jan28_joined, coords = c("X", "Y"), crs = 27700)

# To Map

traffic_unique = ddply(jan28_joined, .(`Geographic Address`), head, n = 1)
unique_spatial = st_as_sf(traffic_unique, coords = c("X", "Y"), crs = 27700)

# Stat19 to motorways only

buffer_unq = st_buffer(unique_spatial, 500)

traf_stat19 = stat19_spatial[buffer_unq, ]

road_types = c("1", "3")

traf_stat19 = traf_stat19[traf_stat19$`1st_Road_Class` %in% road_types,]

buffer_traf = st_buffer(traf_stat19, 500)

# Unique

unique_spatial2 = unique_spatial[buffer_traf, ]

# All Traff data

jan28_spatial_2 = jan28_spatial[buffer_traf, ]

nrow(stat19_spatial)
names(jan28)
?as.numeric
head(stat19)
table(stat19_spatial$Accident_Severity)

fatal = stat19_spatial[stat19_spatial$Accident_Severity == 1, ]

fatalbuffer = st_buffer(fatal, 500)

fataltraffic = jan28_spatial[fatalbuffer,]

library(ggplot2)
library(ggmap)
library(rgdal)

plot = plot(jan28_spatial$geometry, col = "blue") +
plot(stat19_spatial$geometry, col = "red", add = TRUE)


mapview::mapview(traf_stat19) + mapview::mapview(unique_spatial2)

unique(traffic_unique$geometry)
?mapview
