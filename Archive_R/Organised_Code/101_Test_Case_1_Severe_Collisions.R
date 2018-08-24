# Extract of Test sites from Halogen Data

# Run 007_Formatting_Halogen_Data on 28/01/2016

stat19_date = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Stat19_2016_2km_Subset.csv")))

colnames(stat19) = c("Accident_Index", "Location_Easting_OSGR", "Location_Northing_OSGR", "Police_Force", "Accident_Severity", "Number_of_Vehicles", "Number_of_Casualties", "Date", "Day_of_Week", "Time", "Local_Authority_(District)", "Local_Authority_(Highway)", "1st_Road_Class", "1st_Road_Number", "Road_Type", "Speed_limit", "Junction_Detail", "Junction_Control", "2nd_Road_Class", "2nd_Road_Number", "Pedestrian_Crossing-Human_Control", "Pedestrian_Crossing-Physical_Facilities", "Light_Conditions", "Weather_Conditions", "Road_Surface_Conditions", "Special_Conditions_at_Site", "Carriageway_Hazards", "Urban_or_Rural_Area", "Did_Police_Officer_Attend_Scene_of_Accident", "LSOA_of_Accident_Location", "X", "Y")

# Libraries Used

# library(ggplot2)
# library(gridExtra)

jan28 = as.data.frame(halo_spatial[date(halo_spatial$Datetime) == "2016-01-28",])
head(jan28)
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
                   "Datetime", "After", "Before", "X", "Y",
                   "AveSpeed", "AveOccupancy", 
                   "AveHeadway", "TotalFlow")

jan28 = st_as_sf(jan28, coords = c("X", "Y"), crs = 27700)

stat19_date = stat19[stat19$Date == "28/01/2016" & stat19$Accident_Severity %in% c(1,2) & stat19$Junction_Detail == 0 & stat19$Road_Type == 3,]
stat19_date = st_as_sf(stat19_date, coords = c("X", "Y"), crs = 27700)

# Accident Profile M1/2516B - Severity 2 - Downstream

TC1TD = jan28[jan28$`Geographic Address` == "M1/2516B",]
#TC1buffer = st_buffer(TC1TD, 500)
TC1A = stat19_date[stat19_date$Accident_Index == "2016406CA0222",]

#TC1A = stat19_date[TC1buffer,]

# Speed

DSL1 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 1`), colour = "blue") + 
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))
DSL2 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))
DSL3 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))
DSL4 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))

# Occupancy

DOL1 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
DOL2 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
DOL3 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
DOL4 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))

# Headway

DHL1 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 255))
DHL2 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 255))
DHL3 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 255))
DHL4 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 255))

# Total Flow

DTF1 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
DTF2 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
DTF3 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
DTF4 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))


grid.arrange(DSL1, DSL2, DSL3, DSL4,DOL1, DOL2, DOL3, DOL4, DHL1, DHL2, DHL3, DHL4, DTF1, DTF2, DTF3, DTF4, nrow = 4, ncol = 4, top = "M1/2516B Downstream of Collision")

# Accident Profile M1/2519B - Severity 2 - Upstream

TC1TU = jan28[jan28$`Geographic Address` == "M1/2519B",]
#TC1buffer = st_buffer(TC1TU, 500)
TC1A = stat19_date[stat19_date$Accident_Index == "2016406CA0222",]

#TC1A = stat19_date[TC1buffer,]

# Speed

USL1 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))
USL2 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))
USL3 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))
USL4 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))

# Occupancy

UOL1 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
UOL2 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
UOL3 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
UOL4 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))

# Headway

UHL1 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 260))
UHL2 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 260))
UHL3 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 260))
UHL4 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 260))

# Total Flow

UTF1 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
UTF2 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
UTF3 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
UTF4 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))

grid.arrange(USL1, USL2, USL3, USL4,UOL1, UOL2, UOL3, UOL4, UHL1, UHL2, UHL3, UHL4, UTF1, UTF2, UTF3, UTF4, nrow = 4, ncol = 4, top = "M1/2519B Upstream of Collision")

# Accident Profile eg3 - Severity 2 - Rubbernecking

TC1TR = jan28[jan28$`Geographic Address` == "M1/2516A",]
TC1A = stat19_date[stat19_date$Accident_Index == "2016406CA0222",]
#TC1buffer = st_buffer(TC1TR, 500)
#TC1A = stat19_date[TC1buffer,]

# Speed

RSL1 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))
RSL2 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))
RSL3 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))
RSL4 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170))

# Occupancy

ROL1 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
ROL2 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
ROL3 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
ROL4 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))

# Headway

RHL1 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 260))
RHL2 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 260))
RHL3 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 260))
RHL4 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 260))

# Total Flow

RTF1 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 1`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
RTF2 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 2`), colour = "red") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
RTF3 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 3`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))
RTF4 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 4`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100))


grid.arrange(RSL1, RSL2, RSL3, RSL4,ROL1, ROL2, ROL3, ROL4, RHL1, RHL2, RHL3, RHL4, RTF1, RTF2, RTF3, RTF4,
             nrow = 4, ncol = 4, top = "M1/2516A Opposite Carriageway of the Collision")


# Average Speeds of Upstream Flow

USpe = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `AveSpeed`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "red", linetype=2) +
  geom_vline(xintercept= hms(TC1A$Time) + hours(3) + minutes(24), colour = "darkgreen", linetype=2) +
  ylim(c(0, 170))

UOcc = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `AveOccupancy`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "red", linetype=2) +
  geom_vline(xintercept= hms(TC1A$Time) + hours(3) + minutes(24), colour = "darkgreen", linetype=2) + 
  ylim(c(0, 100))

UHea = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `AveHeadway`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "red", linetype=2) +
  geom_vline(xintercept= hms(TC1A$Time) + hours(3) + minutes(24), colour = "darkgreen", linetype=2) + 
  ylim(c(0, 255)) 

UTot = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `TotalFlow`), colour = "darkblue") +
  geom_vline(xintercept= TC1A$Time, colour = "red", linetype=2) +
  geom_vline(xintercept= hms(TC1A$Time) + hours(3) + minutes(24), colour = "darkgreen", linetype=2)

grid.arrange(USpe, UOcc, UHea, UTot, nrow = 1, ncol = 4, top = "M1/2519B Upstream of Collision: Trend of traffic Day After Collision")



USpe = ggplot(data = halo_spatial, aes(x = `Time_GMT`)) +
  geom_point(mapping = aes(y = `AveSpeed`), colour = "blue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 170)) +
  geom_smooth(mapping = aes(y = `AveSpeed`), method = 'loess', colour = "black")

UOcc = ggplot(data = halo_spatial, aes(x = `Time_GMT`)) +
  geom_point(mapping = aes(y = `AveOccupancy`), colour = "green") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 100)) +
  geom_smooth(mapping = aes(y = `AveOccupancy`), method = 'loess', colour = "black")

UHea = ggplot(data = halo_spatial, aes(x = `Time_GMT`)) +
  geom_point(mapping = aes(y = `AveHeadway`), colour = "purple") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  ylim(c(0, 255)) +
  geom_smooth(mapping = aes(y = `AveHeadway`), method = 'loess', colour = "black")

UTot = ggplot(data = halo_spatial, aes(x = `Time_GMT`)) +
  geom_point(mapping = aes(y = `TotalFlow`), colour = "darkblue") +
  geom_vline(xintercept= TC1A$Time, colour = "black", linetype=2) +
  geom_smooth(mapping = aes(y = `TotalFlow`), method = 'loess', colour = "black")

grid.arrange(USpe, UOcc, UHea, UTot, nrow = 1, ncol = 4, top = "MIDAS Gold Dataset: Aggregated Lane Averages and Total Flow per Minute")
