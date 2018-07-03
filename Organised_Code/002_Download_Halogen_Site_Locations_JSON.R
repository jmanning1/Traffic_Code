# Extract Site Locations from Webtris

# Inputs:
#   Webtris

# Outputs:
#   Site_Locations.csv

# Libraries Used:
# library("httr")
# library( "jsonlite")

# MIDAS Sites

sites = fromJSON( "http://webtris.highwaysengland.co.uk/api/v1/sitetypes/1/sites" , simplifyDataFrame=TRUE, flatten = TRUE)

sites = sites$sites
names(sites)
colnames(sites) = c("Id", "Name", "Description", "Longitude", "Latitude", "Status")

# Identify and Highlight Duplicates

dups_unique = sites$Description[duplicated(sites$Description)]
dups = sites[sites$Description %in% dups_unique,]

write.csv(dups, file = "D:/Documents/5872M-Dissertation/Data/Geometries/dups.csv",row.names=FALSE)

# Format Column Names to Align with Halogen Data (Description -> Geographic Address)

colnames(sites) = c("Id", "Name", "Geographic Address", "Longitude", "Latitude", "Status")

# Remove Irrelevant and Inactive Sites

sites = sites[sites$`Geographic Address` != "NO TRADS ID",]
sites = sites[sites$`Geographic Address` != "NO MIDAS ID",]
sites = sites[sites$`Geographic Address` != "NO TMU ID",]
sites = sites[sites$Status != "Inactive",]

# Remove Duplicate Sites

sites = sites[!(duplicated(sites$`Geographic Address`) | duplicated(sites$`Geographic Address`, fromLast = TRUE)), ]

# Remove Sliproads

sites = sites[!grepl("K", sites) & !grepl("J", sites) & !grepl("L", sites) & !endsWith("M", sites)]

# Output Site Locations

write.csv(sites, file = "D:/Documents/5872M-Dissertation/Data/Geometries/Site_Locations.csv",row.names=FALSE)

