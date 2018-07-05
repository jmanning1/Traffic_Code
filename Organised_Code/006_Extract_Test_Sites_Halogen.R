# Extract Wanted Sites using oneminutetrafficdata package.

stat19 = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Stat19_2016_2km_Subset.csv")))

# Extract Severe Accidents on Main Roads, Not Near Complex Networks eg Junctions

severe = c(1,2)

stat19 = as.data.frame(stat19[stat19$Accident_Severity %in% severe & stat19$Road_Type == 3  & stat19$Junction_Detail == 0, ])

# Make Spatial

stat19_spatial = st_as_sf(stat19, coords = c("X", "Y"), crs = 27700)

stat19_buffer = st_buffer(stat19_spatial, 500)

#Extract Sites

sites = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Site_Locations.csv"))

ac_sites = st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

osgb_sites = st_transform(ac_sites, crs = 27700)

osgb_sites = osgb_sites[stat19_buffer, ]

Sites_accidents = unique(osgb_sites$`Geographic Address`)

# Extract Wanted Dates

dates = as.data.frame(unique(stat19_spatial$Date))
colnames(dates) = "Date"
dates_formatted = as.Date(dates[,1], "%d/%m/%Y")
month = dates_formatted[months(dates_formatted) == "March"]
month= as.character(month)

####################Remove after run

'%!in%' <- function(x,y)!('%in%'(x,y)) # https://stackoverflow.com/questions/5831794/opposite-of-in
days_done = c("2016-03-02",
              "2016-03-06",
              "2016-03-08",
              "2016-03-11",
              "2016-03-12",
              "2016-03-13",
              "2016-03-16",
              "2016-03-19",
              "2016-03-21",
              "2016-03-26",
              "2016-03-31")

month = month[month %!in% days_done]

###################################

for(j in seq(length(month))){
  
  
  rd = RoadData(startDate = month[j], endDate = month[j], tcdFileRoot = "D:/Documents/5872M-Dissertation/Data/Halogen_2016",Sites_accidents)
  
  # Make Dataframe
  
  df_list = lapply(seq(length(rd)),function(i){
    df = as.data.frame(rd[i], stringsAsFactors = FALSE)
  })
  
  head(do.call(bind_rows,df_list))
  
  df1 = as.data.frame(data.table::rbindlist(df_list, use.names=TRUE, fill=TRUE))
  
  csv_file = paste0("D:/Documents/5872M-Dissertation/Data/Halogen_Site_Severe/", month[j], ".csv")
  
  # Output to csv
  
  write.csv(df1, file = csv_file, row.names=FALSE)
  
  # Clean up Memory Usage
  
  rm(df1)
  rm(df_list)
  rm(rd)
  gc()
}
