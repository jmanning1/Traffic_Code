# Extract Wanted Sites using oneminutetrafficdata package.


library(oneminutetrafficdata)
library(sf)
library(dplyr)
library(data.table)
library(plyr)

stat19 = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Stat19_2016_2km_Subset.csv")))

stat19_spatial = st_as_sf(stat19, coords = c("X", "Y"), crs = 27700)

class(stat19_spatial) # Should be:  "sf"         "data.frame"

# Run 004_Subset_Traffic_Sites_By_Proximity_To_Collisions.R 

# Extract Wanted Dates

Sites_accidents = unique(osgb_sites$`Geographic Address`)
dates = as.data.frame(unique(stat19_spatial$Date))
colnames(dates) = "Date"
dates_formatted = as.Date(dates[,1], "%d/%m/%Y")
month = dates_formatted[months(dates_formatted) == "February"]
month= as.character(month)

for(j in seq(length(month))){
  
  
  rd = RoadData(startDate = month[j], endDate = month[j], tcdFileRoot = "D:/Documents/5872M-Dissertation/Data/Halogen_2016",Site_Accidents_reduce)
  
  # Make Dataframe
  
  df_list = lapply(seq(length(rd)),function(i){
    df = as.data.frame(rd[i], stringsAsFactors = FALSE)
  })
  
  head(do.call(bind_rows,df_list))
  
  df1 = as.data.frame(data.table::rbindlist(df_list, use.names=TRUE, fill=TRUE))
  
  csv_file = paste0("D:/Documents/5872M-Dissertation/Data/Halogen_Site_csv/", month[j], ".csv")
  
  # Output to csv
  
  write.csv(df1, file = csv_file, row.names=FALSE)
  
  # Clean up Memory Usage
  
  rm(df1)
  rm(df_list)
  rm(rd)
  gc()
}