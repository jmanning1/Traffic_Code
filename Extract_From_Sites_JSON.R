library("httr")
library( "jsonlite")

sites = fromJSON( "http://webtris.highwaysengland.co.uk/api/v1/sites" , simplifyDataFrame=TRUE, flatten = TRUE)
sites = sites$sites
names(sites)
colnames(sites) = c("Id", "Name", "Description", "Longitude", "Latitude", "Status")

write.csv(sites, file = "D:/Documents/5872M-Dissertation/Data/Geometries/Site_Locations.csv",row.names=FALSE)

# Identify and Highlight Duplicates

dups_unique = sites$Description[duplicated(sites$Description)]
dups = sites[sites$Description %in% dups_unique,]

write.csv(dups, file = "D:/Documents/5872M-Dissertation/Data/Geometries/dups.csv",row.names=FALSE)