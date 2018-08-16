devtools::install_github("RACFoundation/oneminutetrafficdata")
library(oneminutetrafficdata)


rd = RoadData(startDate = "2015-07-01", endDate = "2015-07-01", tcdFileRoot = "tcd", "M")
# rd = RoadData(startDate = "2015-07-01", endDate = "2015-07-01", tcdFileRoot = "tcd", "M25/4393A", "M25/4393B") # fail
rd = RoadData(startDate = "2015-07-01", endDate = "2015-07-01", tcdFileRoot = "tcd", "M1/4394A") # fail
oneminutetrafficdata::ExtractData(fileNames = )

R.utils::bunzip2("tcd-data/10020715.tcd.bz2", "10020715.tcd")

dates <- seq(as.Date("2015-07-01"), as.Date("2015-07-01"), by = "days")
d = "tcd"
f_tcd = list.files(path = d, pattern = "*.tcd$", full.names = TRUE)
file.exists(f_tcd)

f_tcd = gsub(pattern = ".tcd", replacement = "", x = f_tcd)
res = oneminutetrafficdata::ExtractData(fileNames = f_tcd, sites = c("M25/4393A", "M1"), dates)
f_csv = paste0(f_tcd, ".csv")

library(tidyverse)
res = map_df(f_csv[1], read_csv)

# with stats19 data
library(sf)
library(stplanr)
stplanr::dl_stats19()
devtools::install_github("robinlovelace/ukboundaries")
library(ukboundaries)
library(osmdata)
library(stplanr)
library(sf)
dl_stats19()
a = read_stats19_ac()
names(a)
a_no_na = a[!is.na(a$Latitude), ]
a_sf = st_as_sf(a_no_na, coords = c("Longitude", "Latitude"), crs = 4326)
m = opq("Leeds") %>% 
  add_osm_feature("highway", "motorway") %>% 
  osmdata_sf()
plot(m$osm_lines$geometry)
m_buff = st_transform(m$osm_lines, 27700) %>% 
  st_buffer(1000) %>% 
  st_transform(4326)
plot(m_buff, add = T)
am = a_sf[m_buff, ]
plot(am$geometry)

#> although coordinates are longitude/latitude, st_intersects assumes that they are planar
plot(st_geometry(a_leeds))

# extractedData <- ExtractData(fileNames, sites, dates)
# oneminutetrafficdata::RoadData(startDate = "2018-06-01", endDate = "2018-06-01", tcdFileRoot = "/tmp/trafficdata")
# oneminutetrafficdata::DownloadCSV(data = )
# oneminutetrafficdata::ExtractData(fileNames = )
# 
# 
# download.file(url = "https://www.midas-data.org.uk/midasdata/Trafdata/Co10/2018/May/10300518.tcd.bz2", "/tmp/1.tcd.bz2")
# 

# 
# # raw data
# file = f_tcd[1]
# address           <- readChar(file, nchars = 12, useBytes = FALSE)
# electronicAddress <- readBin(file, "integer", n = 1, size = 4)
# numberOfLanes     <- readBin(file, "integer", n = 1, size = 1, signed = FALSE)
# dataBulk <- readBin(file, "integer", n = 1440 * (8 + (numberOfLanes * 5)), size = 1, signed = FALSE)
# dim(dataBulk) <- c(8 + numberOfLanes*5, 1440)
# dataBulk <- t(dataBulk)
# 
# flowPerCategories <- dataBulk[, c(2,4,6,8)]
# allData <- cbind(controlOfficeNumber, address, year, month, day, dayOfWeek, typeOfDay, daysAfterNearestBankHoliday, time, numberOfLanes, flowPerCategories)
# 
# colnames(allData)[1:14] <- c("Control Office", "Geographic Address", "Year", "Month", "Day", "Day of Week", "Type of Day", "Days After Nearest Bank Holiday", "Time (GMT)", "Number of Lanes", "Flow (Category 1)", "Flow (Category 2)", "Flow (Category 3)", "Flow (Category 4)")
# 
# for (i in 1:numberOfLanes) {
#   
#   attributes <- dataBulk[, 5*(i - 1) + c(9, 11, 12, 13)]
#   allData <- cbind(allData, attributes)
#   
#   colnames(allData)[dim(allData)[2]-3] <- paste("Average Speed (Lane ", i, ")", sep = "")
#   colnames(allData)[dim(allData)[2]-2] <- paste("Total Flow (Lane ", i, ")", sep = "")
#   colnames(allData)[dim(allData)[2]-1] <- paste("Occupancy (Lane ", i, ")", sep = "")
#   colnames(allData)[dim(allData)[2]] <- paste("Average Headway (Lane ", i, ")", sep = "")
#   
# }
