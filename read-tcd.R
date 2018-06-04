devtools::install_github("RACFoundation/oneminutetrafficdata")
library(oneminutetrafficdata)


dates <- seq(as.Date("2018-06-01"), as.Date("2018-06-01"), by = "days")
d = "/tmp/trafficdata/"
f_tcd = list.files(path = d, pattern = "*.tcd", full.names = TRUE)
f_tcd = gsub(pattern = ".tcd", replacement = "", x = f_tcd)
oneminutetrafficdata::ExtractData(fileNames = f_tcd, sites = "M25/4393A", dates)
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
