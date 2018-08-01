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

a <- character(0)

# Cycle through Accidents

for(j in 1:nrow(stat19_buffer)){
  collision = stat19_buffer[j, ]
  current_sites = osgb_sites[stat19_buffer[j,], ]
  Sites_accidents = unique(current_sites$`Geographic Address`)
  if(identical(a, Sites_accidents) == FALSE){
    day_before = as.character(dmy(collision$Date) - days(1))
    day_after = as.character(dmy(collision$Date) + days(1))
    rd = RoadData(startDate = day_before, endDate = day_after, tcdFileRoot = "D:/Documents/5872M-Dissertation/Data/Halogen_2016",Sites_accidents)
  
    # Make Dataframe
  
    df_list = lapply(seq(length(rd)),function(i){
    df = as.data.frame(rd[i], stringsAsFactors = FALSE)
    })
  
    head(do.call(bind_rows,df_list))
  
    df1 = as.data.frame(data.table::rbindlist(df_list, use.names=TRUE, fill=TRUE))
  
    csv_file = paste0("D:/Documents/5872M-Dissertation/Data/Halogen_Severe_window_2km/", collision$Accident_Index, ".csv")
  
    # Output to csv
  
    write.csv(df1, file = csv_file, row.names=FALSE)
  
    # Clean up Memory Usage
  
    rm(df1)
    rm(df_list)
    rm(rd)
    gc()
  }
}
