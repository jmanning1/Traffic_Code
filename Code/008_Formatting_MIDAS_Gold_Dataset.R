# Format Halogen Data

# Load Stat19 Data for Halogen Slimming

stat19 = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Stat19_2016_2km_Subset.csv")))

colnames(stat19) = c("Accident_Index", "Location_Easting_OSGR", "Location_Northing_OSGR", "Police_Force", "Accident_Severity", "Number_of_Vehicles", "Number_of_Casualties", "Date", "Day_of_Week", "Time", "Local_Authority_(District)", "Local_Authority_(Highway)", "1st_Road_Class", "1st_Road_Number", "Road_Type", "Speed_limit", "Junction_Detail", "Junction_Control", "2nd_Road_Class", "2nd_Road_Number", "Pedestrian_Crossing-Human_Control", "Pedestrian_Crossing-Physical_Facilities", "Light_Conditions", "Weather_Conditions", "Road_Surface_Conditions", "Special_Conditions_at_Site", "Carriageway_Hazards", "Urban_or_Rural_Area", "Did_Police_Officer_Attend_Scene_of_Accident", "LSOA_of_Accident_Location", "X", "Y")

stat19 = st_as_sf(stat19, coords = c("X", "Y"), crs = 27700)

# Highest Accident Dates

severe = c(1,2)
#data = stat19[stat19$Accident_Severity %in% severe & stat19$Road_Type == 3  & stat19$Junction_Detail == 0, ]

#st_crs(data) = 27700

# Visualisation

# af = tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
#   tm_polygons(alpha = 0.1) +
#   tm_shape(data) +
#   tm_dots() + 
#   tm_layout(title = "Reduced Collisions \n by Accident Severity, \n Road Type and \n Junction Detail", main.title.size = 10)
# 
# grid.newpage()
# pushViewport(viewport(layout=grid.layout(1,2)))
# print(ae, vp=viewport(layout.pos.col = 1))
# print(af, vp=viewport(layout.pos.col = 2))

# tab = as.data.frame(table(data$Date))
# tab = tab[order(-tab$Freq, decreasing = FALSE),] 

# Pick Date of Collision
# #Specific Day
# stat19_date = stat19[stat19$Date == "28/01/2016" & stat19$Accident_Severity %in% severe & stat19$Junction_Detail == 0 & stat19$Road_Type == 3,]
# 
# # Specific Month
# stat19_date = stat19[month(stat19$Date) == 3 & stat19$Accident_Severity %in% severe & stat19$Junction_Detail == 0 & stat19$Road_Type == 3,]
# 

# Severe and Fatal Accidents Not near Junction and on Carriageway
stat19_date = stat19[stat19$Accident_Severity %in% severe & stat19$Junction_Detail == 0 & stat19$Road_Type == 3,]



stat19_date = st_as_sf(stat19_date, coords = c("X", "Y"), crs = 27700)
buffer_stat19 = st_buffer(stat19_date, 500)

# # halo = Halogen Dataset
# 
# halo = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Halogen_Site_csv/", "2016-01-28.csv")))
# 
# halo = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Halogen_Site_csv/", "2016-01-09.csv")))
# 
# halo = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Halogen_Site_csv/", "2016-02-24.csv")))
# 
# halo = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Halogen_Site_csv/", "2016-02-07.csv")))
# 


# Correct Columns

colnames(halo) = c("Control Office","Geographic Address",
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
                   "Occupancy Lane 5","Average Headway Lane 5")


# colnames(halo) = c("Control Office","Geographic Address",
#                    "Year","Month",
#                    "Day","Day of Week",
#                    "Type of Day","Days After Nearest Bank Holiday",
#                    "Time GMT","Number of Lanes",
#                    "Flow Category 1","Flow Category 2",
#                    "Flow Category 3","Flow Category 4",
#                    "Average Speed Lane 1","Total Flow Lane 1",
#                    "Occupancy Lane 1","Average Headway Lane 1",
#                    "Average Speed Lane 2","Total Flow Lane 2",
#                    "Occupancy Lane 2","Average Headway Lane 2",
#                    "Average Speed Lane 3","Total Flow Lane 3",
#                    "Occupancy Lane 3","Average Headway Lane 3",
#                    "Average Speed Lane 4","Total Flow Lane 4",
#                    "Occupancy Lane 4","Average Headway Lane 4",
#                    "Average Speed Lane 5","Total Flow Lane 5",
#                    "Occupancy Lane 5","Average Headway Lane 5",
#                    "Average Speed Lane 6","Total Flow Lane 6",
#                    "Occupancy Lane 6","Average Headway Lane 6")

halo$`Geographic Address` = trimws(halo$`Geographic Address`)

head(halo$`Geographic Address`)

# Get Site Geoms

sites = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Site_Locations.csv"))

ac_sites = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

osgb_sites = st_transform(ac_sites, crs = 27700)

coor = st_coordinates(osgb_sites$geometry)

coor = as.data.frame(cbind(osgb_sites$`Geographic Address`, coor))

colnames(coor) = c("Geographic Address","X","Y")

coor$X = as.numeric(as.character(coor$X))

coor$Y =  as.numeric(as.character(coor$Y))

coor$`Geographic Address` = as.character(coor$`Geographic Address`)

# Run if Testing to get reduced Halogen Points

# sites_nc = osgb_sites[buffer_stat19, ]
# sites_nc_buffer = st_buffer(sites_nc, 500)
# stat19_nt = stat19_date[sites_nc_buffer,]
# stat19_date = stat19_nt
# bk = halo
# halo = halo[halo$`Geographic Address` %in% sites_nc$`Geographic Address`,]

# Join Co-oridinates to Halogen Data

halo = left_join(halo, coor, by = "Geographic Address")

# sapply(halo, function(x) sum(is.na(x)))

# Run if NA's in co-ordinates

# halo = halo[is.na(halo$X) == FALSE & is.na(halo$Y) == FALSE,]

# Format Halogen Dataset
halo$Month = as.character(halo$Month)
halo$Day = as.integer(as.character(halo$Day))
halo$Year = as.integer(as.character(halo$Year))
halo$`Average Speed Lane 3` = as.integer(halo$`Average Speed Lane 3`)
halo$`Total Flow Lane 3` = as.integer(halo$`Total Flow Lane 3`)
halo$`Occupancy Lane 3` = as.integer(halo$`Occupancy Lane 3`)
halo$`Average Headway Lane 3` = as.integer(halo$`Average Headway Lane 3`)
halo$`Average Speed Lane 4` = as.integer(halo$`Average Speed Lane 4`)
halo$`Total Flow Lane 4` = as.integer(halo$`Total Flow Lane 4`)
halo$`Occupancy Lane 4` = as.integer(halo$`Occupancy Lane 4`)
halo$`Average Headway Lane 4` = as.integer(halo$`Average Headway Lane 4`)
halo$`Average Speed Lane 5` = as.integer(halo$`Average Speed Lane 5`)
halo$`Total Flow Lane 5` = as.integer(halo$`Total Flow Lane 5`)
halo$`Occupancy Lane 5` = as.integer(halo$`Occupancy Lane 5`)
halo$`Average Headway Lane 5` = as.integer(halo$`Average Headway Lane 5`)
# halo$`Average Speed Lane 6` = as.integer(halo$`Average Speed Lane 6`)
# halo$`Total Flow Lane 6` = as.integer(halo$`Total Flow Lane 6`)
# halo$`Occupancy Lane 6` = as.integer(halo$`Occupancy Lane 6`)
# halo$`Average Headway Lane 6` = as.integer(halo$`Average Headway Lane 6`)

# Set NA values to value indicating lack of data - Headway = 255, Speed = 0, Occupancy = 0, Total Flow = 0

# Speed
halo$`Average Speed Lane 1`[is.na(halo$`Average Speed Lane 1`) == TRUE] = 0
halo$`Average Speed Lane 2`[is.na(halo$`Average Speed Lane 2`) == TRUE] = 0
halo$`Average Speed Lane 3`[is.na(halo$`Average Speed Lane 3`) == TRUE] = 0
halo$`Average Speed Lane 4`[is.na(halo$`Average Speed Lane 4`) == TRUE] = 0
halo$`Average Speed Lane 5`[is.na(halo$`Average Speed Lane 5`) == TRUE] = 0

# Headway

halo$`Average Headway Lane 1`[is.na(halo$`Average Headway Lane 1`) == TRUE] = 255
halo$`Average Headway Lane 2`[is.na(halo$`Average Headway Lane 2`) == TRUE] = 255
halo$`Average Headway Lane 3`[is.na(halo$`Average Headway Lane 3`) == TRUE] = 255
halo$`Average Headway Lane 4`[is.na(halo$`Average Headway Lane 4`) == TRUE] = 255
halo$`Average Headway Lane 5`[is.na(halo$`Average Headway Lane 5`) == TRUE] = 255

# Occupancy

halo$`Occupancy Lane 1`[is.na(halo$`Occupancy Lane 1`) == TRUE] = 0
halo$`Occupancy Lane 2`[is.na(halo$`Occupancy Lane 2`) == TRUE] = 0
halo$`Occupancy Lane 3`[is.na(halo$`Occupancy Lane 3`) == TRUE] = 0
halo$`Occupancy Lane 4`[is.na(halo$`Occupancy Lane 4`) == TRUE] = 0
halo$`Occupancy Lane 5`[is.na(halo$`Occupancy Lane 5`) == TRUE] = 0

# Total Flow

halo$`Total Flow Lane 1`[is.na(halo$`Total Flow Lane 1`) == TRUE] = 0
halo$`Total Flow Lane 2`[is.na(halo$`Total Flow Lane 2`) == TRUE] = 0
halo$`Total Flow Lane 3`[is.na(halo$`Total Flow Lane 3`) == TRUE] = 0
halo$`Total Flow Lane 4`[is.na(halo$`Total Flow Lane 4`) == TRUE] = 0
halo$`Total Flow Lane 5`[is.na(halo$`Total Flow Lane 5`) == TRUE] = 0


# Test for NA's

# sapply(halo, function(x) sum(is.na(x)))

# Make Spatial

halo_spatial = st_as_sf(halo, coords = c("X", "Y"), crs = 27700)

# Visualisation

# mapview::mapview(sites_nc) + mapview::mapview(collisions_today)



