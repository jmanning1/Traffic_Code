library(lubridate)

# On Traffic Give Indication of Crash within occured nearby - 500m

stat19_date$Datetime = ymd_hms(as.character(as.POSIXct(paste(stat19_date$Date, stat19_date$Time), format = "%d/%m/%Y %H:%M:%S")))

# create a buffer for all accidents

collisions_today = st_buffer(stat19_date, 500)

# Add New Columns for Random Forest Training Tests - 0 = False, 1 = True

halo_spatial$Datetime = ymd_hms(as.character(as.POSIXct(paste(paste0(halo_spatial$Year, "-", halo_spatial$Month, "-", halo_spatial$Day),halo_spatial$`Time GMT`), format="%Y-%b-%d %H:%M:%S")))

# Identify Intervals

before_interval <- as.interval(3600, halo_spatial$Datetime)
# flip interval to get lower date first in the interval
after_interval <- int_flip(as.interval(-3600, halo_spatial$Datetime)) 

# Time and Space 

After = NULL
AfterLoop = NULL
Before = NULL
BeforeLoop = NULL

for(i in seq(length(collisions_today$Datetime))) {
  cir = st_buffer(collisions_today[i,], 500)
  cir = st_union(cir)
  inter = st_intersects(halo_spatial,cir)
  inter_logical = as.logical(inter)
  inter_logical[is.na(inter_logical)] = FALSE
  After <- collisions_today$Datetime[i] %within% after_interval & inter_logical == TRUE
  After = as.integer(After)
  if(i == 1){
    AfterLoop = After
  } else {
    AfterLoop = AfterLoop + After}
  Before <- collisions_today$Datetime[i] %within% before_interval & inter_logical == TRUE
  Before = as.integer(Before)
  if(i == 1){
    BeforeLoop = Before
  } else {
    BeforeLoop = BeforeLoop + Before}
  #jan28_spatial_2$After <- collisions_today$Datetime[i] %within% after_interval & inter_logical == TRUE
  #jan28_spatial_2$Before <- collisions_today$Datetime[i] %within% before_interval & inter_logical == TRUE
}
halo_spatial$After = AfterLoop
halo_spatial$Before = BeforeLoop

write.csv(halo_spatial, file = "D:/Documents/5872M-Dissertation/Data/Geometries/test_Accident_Highlighting.csv",row.names=FALSE)
