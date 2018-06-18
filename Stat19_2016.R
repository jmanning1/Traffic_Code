library(sf)                                                                                   

library(ukboundaries)                                                                         

dl_stats19_2016 <- function(zip_url = paste0("http://data.dft.gov.uk/road-accidents-safety-data/",
                                             "dftRoadSafety_Accidents_2016.zip"), data_dir = tempdir()){
  
  # download and unzip the data if it's not present
  if(!"dftRoadSafety_Accidents_2016.csv" %in% list.files(data_dir)){
    destfile <- file.path(data_dir, "dftRoadSafety_Accidents_2016.zip")
    download.file(zip_url, destfile)
    unzip(destfile, exdir = data_dir)
  }
  
  print(paste0("Data saved at: ", list.files(data_dir,
                                             pattern = "csv", full.names = TRUE)))
  
}

dl_stats19_2016()

# dl_stats19 <- function(zip_url = paste0("http://data.dft.gov.uk.s3.amazonaws.com/",
#                                          "road-accidents-safety-data/Stats19_Data_2005-2014.zip"), data_dir = tempdir()){
# 
#    # download and unzip the data if it's not present
#    if(!"Accidents0514.csv" %in% list.files(data_dir)){
#      destfile <- file.path(data_dir, "Stats19_Data_2005-2014.zip")
#      download.file(zip_url, destfile)
#      unzip(destfile, exdir = data_dir)
#    }
# 
#   print(paste0("Data saved at: ", list.files(data_dir,
#                                              pattern = "csv", full.names = TRUE)))
# }
# 
# dl_stats19()

# Read File

ac = read.csv(file.path(tempdir(), "dftRoadSafety_Accidents_2016.csv")) 
                                   
### Format File

colnames(ac) = c("Accident_Index", "Location_Easting_OSGR", "Location_Northing_OSGR", "Longitude", "Latitude", "Police_Force", "Accident_Severity", "Number_of_Vehicles", "Number_of_Casualties", "Date", "Day_of_Week", "Time", "Local_Authority_(District)", "Local_Authority_(Highway)", "1st_Road_Class", "1st_Road_Number", "Road_Type", "Speed_limit", "Junction_Detail", "Junction_Control", "2nd_Road_Class", "2nd_Road_Number", "Pedestrian_Crossing-Human_Control", "Pedestrian_Crossing-Physical_Facilities", "Light_Conditions", "Weather_Conditions", "Road_Surface_Conditions", "Special_Conditions_at_Site", "Carriageway_Hazards", "Urban_or_Rural_Area", "Did_Police_Officer_Attend_Scene_of_Accident", "LSOA_of_Accident_Location")


format_stats19_ac_2016 <- function(ac){
  
  ac$Accident_Severity <-
    factor(ac$Accident_Severity, labels = c("Fatal", "Serious", "Slight"))
  ac$Police_Force <-
    factor(ac$Police_Force,
           labels =
             c("Metropolitan Police", "Cumbria", "Lancashire", "Merseyside",
               "Greater Manchester", "Cheshire", "Northumbria", "Durham", "North Yorkshire",
               "West Yorkshire", "South Yorkshire", "Humberside", "Cleveland",
               "West Midlands", "Staffordshire", "West Mercia", "Warwickshire",
               "Derbyshire", "Nottinghamshire", "Lincolnshire", "Leicestershire",
               "Northamptonshire", "Cambridgeshire", "Norfolk", "Suffolk", "Bedfordshire",
               "Hertfordshire", "Essex", "Thames Valley", "Hampshire", "Surrey",
               "Kent", "Sussex", "City of London", "Devon and Cornwall", "Avon and Somerset",
               "Gloucestershire", "Wiltshire", "Dorset", "North Wales", "Gwent",
               "South Wales", "Dyfed-Powys", "Northern", "Grampian", "Tayside",
               "Fife", "Lothian and Borders", "Central", "Strathclyde", "Dumfries and Galloway"
             ))
  ac$`1st_Road_Class` <-
    factor(ac$`1st_Road_Class`,
           labels = c("Motorway", "A(M)", "A", "B", "C", "Unclassified"))
  
  # Missing Level added as in 2016, not in 2005-14
  ac$Road_Type <-
    factor(ac$Road_Type,
           labels = c("Missing", "Roundabout", "One way street", "Dual carriageway", "Single carriageway",
                      "Slip road", "Unknown"))
  ac$Junction_Detail <-
    factor(ac$Junction_Detail, labels =
             c("Data missing or out of range", "Not at junction or within 20 metres",
               "Roundabout", "Mini-roundabout", "T or staggered junction", "Slip road",
               "Crossroads", "More than 4 arms (not roundabout)", "Private drive or entrance",
               "Other junction"))
  
  # Missing Level added as in 2016, not in 2005-14
  ac$Light_Conditions <-
    factor(ac$Light_Conditions,
           labels = c("Missing", "Daylight", "Darkness - lights lit", "Darkness - lights unlit",
                      "Darkness - no lighting", "Darkness - lighting unknown"))
  ac$Weather_Conditions <-
    factor(ac$Weather_Conditions,
           labels = c("Data missing or out of range", "Fine no high winds", "Raining no high winds",
                      "Snowing no high winds", "Fine + high winds", "Raining + high winds",
                      "Snowing + high winds", "Fog or mist", "Other", "Unknown"))
  ac$Road_Surface_Conditions <-
    factor(ac$Road_Surface_Conditions,
           labels = c("Data missing or out of range", "Dry", "Wet or damp", "Snow",
                      "Frost or ice", "Flood over 3cm. deep"))
  ac$Time <-
    lubridate::hm(ac$Time)
  # hist(ac$Time@hour) # verify times
  ac$Date <- lubridate::dmy(ac$Date)
  # barplot(table(lubridate::wday(ac$Date, label = TRUE)))
  
  names(ac)[1] <- "Accident_Index" # rename faulty index name
  
  ac
  
}

format_stats19_ac_2016(ac)

# Extract Leeds

ac = ac[!is.na(ac$Longitude), ]                                                               
ac = st_as_sf(ac, coords = c("Longitude", "Latitude"), crs = 4326)                            
ac_lds = ac[leeds, ]       
head(ac_lds)

plot(ac_lds$Location_Easting_OSGR,ac_lds$Location_Northing_OSGR)



####################################
# 
# library(oneminutetrafficdata)
# 
# library(ukboundaries)
# library(stplanr)
# library(sf)
# 
# dl_stats19()
# 
# a = read_stats19_ac()
# 
# data
# 
# b = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/dftRoadSafety_Accidents_2016/","dftRoadSafety_Accidents_2016.csv"))
# 
# names(b)
# 
# head(b)
# 
# a_no_na = a[!is.na(b$Latitude), ]
# a_sf = st_as_sf(a_no_na, coords = c("Longitude", "Latitude"), crs = 4326)
# m = opq("Leeds") %>% 
#   add_osm_feature("highway", "motorway") %>% 
#   osmdata_sf()
# plot(m$osm_lines$geometry)
# m_buff = st_transform(m$osm_lines, 27700) %>% 
#   st_buffer(1000) %>% 
#   st_transform(4326)
# plot(m_buff, add = T)
# am = a_sf[m_buff, ]
# plot(am$geometry)