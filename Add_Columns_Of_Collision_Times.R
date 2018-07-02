library(lubridate)

# On Traffic Give Indication of Crash within occured nearby - 500m

# Run Get_Time_Cross_Section.R first up to identified point

nrow(jan28_spatial)

unique(jan28_spatial$`Geographic Address`)

# Strip of Slip roads

jan28_spatial_2 = jan28_spatial[!grepl("K", jan28_spatial$`Geographic Address`) & !grepl("J", jan28_spatial$`Geographic Address`) & !grepl("L", jan28_spatial$`Geographic Address`) & !endsWith("M", jan28_spatial$`Geographic Address`), ]

nrow(jan28_spatial_2)

# See nearby accidents

unique(jan28_spatial_2$`Geographic Address`)

collisons_buffer = st_buffer(jan28_spatial_2, 500)

collisions_today = stat19_spatial[collisons_buffer,]

collisions_today$Datetime = ymd_hms(as.character(as.POSIXct(paste(collisions_today$Date, collisions_today$Time), format = "%d/%m/%Y %H:%M:%S")))

# create a buffer for all accidents

stat19_buffers = st_buffer(stat19_spatial, 500)

# Add New Columns for Random Forest Training Tests - 0 = False, 1 = True

jan28_spatial_2$Datetime = ymd_hms(as.character(as.POSIXct(paste(paste0(jan28_spatial_2$Year, "-", jan28_spatial_2$Month, "-", jan28_spatial_2$Day),jan28_spatial_2$`Time GMT`), format="%Y-%b-%d %H:%M:%S")))
jan28_spatial_2$Imminent_10min = 0
jan28_spatial_2$Imminent_30min = 0
jan28_spatial_2$Occured_1hr = 0
jan28_spatial_2$Occured_3hr = 0
jan28_spatial_2$Occured_6hr = 0
jan28_spatial_2$Occured_12hr = 0
jan28_spatial_2$Imminent_10min = 0
jan28_spatial_2$Imminent_30min = 0
jan28_spatial_2$Imminent_1hr = 0
jan28_spatial_2$Imminent_3hr = 0
jan28_spatial_2$Imminent_6hr = 0
jan28_spatial_2$Imminent_12hr = 0
head(jan28_spatial_2)

# Test times aren't broken

eg3 = jan28_spatial_2[jan28_spatial_2$`Geographic Address` == "M1/2519B",]
eg3$`Average Speed Lane 3` = as.integer(eg3$`Average Speed Lane 3`)
eg3$`Average Speed Lane 4` = as.integer(eg3$`Average Speed Lane 4`)

unique(eg3$`Average Speed Lane 4`)

p5 = ggplot(data = eg3, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 1`), colour = "blue") +
  geom_point(data = eg4, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 170))
p6 = ggplot(data = eg3, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 2`), colour = "red") +
  geom_point(data = eg4, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
p7 = ggplot(data = eg3, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 3`), colour = "green") +
  geom_point(data = eg4, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
p9 = ggplot(data = eg3, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 4`), colour = "yellow") +
  geom_point(data = eg4, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))

grid.arrange(p5, p6, p7, p9, nrow = 1)

# Assign 1's to new columns if true

for(i in seq(nrow(collisions_today))){
  buffer = NULL
  traffic_nearby = NULL
  buffer = st_buffer(collisions_today[i], 500)
  traffic_nearby = jan28_spatial_2[buffer, ]
  for(j in seq(nrow(traffic_nearby))){
    if(traffic_nearby$Datetime[j] %in% seq(from = collisions_today$Datetime[i], to = collisions_today$Datetime[i] + hours(1), by = "hour" )){
      if(traffic_nearby$)
      traffic_nearby$Occured_1hr[j] = 1
    }
  }
}

between(jan28_spatial_2$Datetime[j], collisions_today$Datetime[i], collisions_today$Datetime[i] + hours(1))

eg = jan28_spatial_2[jan28_spatial_2$`Geographic Address` == "M1/2519B",]
buffer = st_buffer(eg, 500)
traffic_nearby = collisions_today[buffer, ]


eg$After = as.integer(eg$After)
eg$Before = as.integer(eg$Before)


plot(eg$Datetime, eg$Before)

after_interval <- as.interval(3600, jan28_spatial_2$Datetime)
# flip interval to get lower date first in the interval
before_interval <- int_flip(as.interval(-3600, jan28_spatial_2$Datetime)) 

# Time only

for(i in seq(length(collisions_today$Datetime))) {
    jan28_spatial_2$After <- collisions_today$Datetime[i] %within% after_interval
    jan28_spatial_2$Before <- collisions_today$Datetime[i] %within% before_interval
}

# Time and Space 

for(i in seq(length(collisions_today$Datetime))) {
  buffer = st_buffer(collisions_today[i], 500)
  if(isTRUE(st_intersects(buffer, jan28_spatial_2))){
  jan28_spatial_2$After <- collisions_today$Datetime[i] %within% after_interval
  }
  if(st_intersects(buffer, jan28_spatial_2) == TRUE){
    jan28_spatial_2$Before <- collisions_today$Datetime[i] %within% before_interval
  }
}

table(jan28_spatial_2$After)
table(jan28_spatial_2$Before)
