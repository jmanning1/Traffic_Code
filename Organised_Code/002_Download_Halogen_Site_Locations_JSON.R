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

all_sts = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)
all_sts = st_transform(all_sts, crs = 27700)
nrow(all_sts)

sts1 = tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(all_sts) +
  tm_dots() + 
  tm_layout(title = "Original MIDAS Webris Sites", main.title.size = 10)

# Identify and Highlight Duplicates

dups_unique = sites$Description[duplicated(sites$Description)]
dups = sites[sites$Description %in% dups_unique,]

# write.csv(dups, file = "D:/Documents/5872M-Dissertation/Data/Geometries/dups.csv",row.names=FALSE)

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

# sites = sites[!grepl("^.+(K)$", sites$`Geographic Address`) & !grepl("^.+(J)$", sites$`Geographic Address`) & !grepl("^.+(L)$", sites$`Geographic Address`) & !grepl("^.+(M)$",sites$`Geographic Address`),]

sites = sites[grepl("^.+(A)$", sites$`Geographic Address`) | grepl("^.+(B)$", sites$`Geographic Address`),]


# Output Site Locations

# write.csv(sites, file = "D:/Documents/5872M-Dissertation/Data/Geometries/Site_Locations.csv",row.names=FALSE)

