library(oneminutetrafficdata)
library(ukboundaries)
library(stplanr)
library(sf)
library(stringr)
library(compare)
library(dplyr)

sites = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Site_Locations.csv"))
colnames(sites) = c("Id", "Name", "Geographic Address", "Longitude", "Latitude", "Status")

# Get Rid of Duplicates from sites

sites = sites[sites$`Geographic Address` != "NO TRADS ID",]
sites = sites[sites$`Geographic Address` != "NO MIDAS ID",]
sites = sites[sites$`Geographic Address` != "NO TMU ID",]
sites = sites[sites$Status != "Inactive",]
sites = sites[!(duplicated(sites$`Geographic Address`) | duplicated(sites$`Geographic Address`, fromLast = TRUE)), ]

# Load Halogen Data

data = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/csv_files/", "10010518.tcd.csv"))

# Get List of Unique locations in data

unique_loc = as.data.frame(unique(data$'Geographic Address'))

colnames(unique_loc) = 'Geographic Address'

unique_loc$`Geographic Address` = as.character(unique_loc$`Geographic Address`)

Locations = right_join(sites, unique_loc, by = "Geographic Address")


head(Locations)
Locations_Missing = Locations[is.na(Locations$Longitude),]
Locations_Missing

# Merge Location with Halogen

Locations = Locations[, c("Geographic Address","Longitude","Latitude")]

Halogen_Geo = right_join(Locations, data, by = "Geographic Address")

plot(Halogen_Geo$Latitude, Halogen_Geo$Longitude)




