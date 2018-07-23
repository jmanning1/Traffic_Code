library(lubridate)

# On Traffic Give Indication of Crash within occured nearby - 500m

stat19_date$Datetime = ymd_hms(as.character(as.POSIXct(paste(stat19_date$Date, stat19_date$Time), format = "%d/%m/%Y %H:%M:%S")))

# create a buffer for all accidents

collisions_today = stat19_date

# Add New Columns for Random Forest Training Tests - 0 = False, 1 = True

head(halo_spatial$`Time GMT`)

halo_spatial$`Time GMT` = as.character(halo_spatial$`Time GMT`)

# data.frame(names=halo_spatial$`Time GMT`,chr=apply(halo_spatial,9,nchar)[,9])

# If Time GMT including Seconds

halo_spatial$Datetime = ymd_hms(as.character(as.POSIXct(paste(paste0(halo_spatial$Year, "-", halo_spatial$Month, "-", halo_spatial$Day), halo_spatial$`Time GMT`), format="%Y-%b-%d %H:%M:%S")))

# If Time GMT missing seconds

halo_spatial$Datetime = ymd_hms(as.character(as.POSIXct(paste(paste0(halo_spatial$Year, "-", halo_spatial$Month, "-", halo_spatial$Day),paste0(halo_spatial$`Time GMT`, ":00")), format="%Y-%b-%d %H:%M:%S")))

# Identify Intervals

before_interval <- as.interval(900, halo_spatial$Datetime)
# flip interval to get lower date first in the interval
after_interval <- int_flip(as.interval(-900, halo_spatial$Datetime)) 

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
  After <- collisions_today$Datetime[i] %within%  after_interval  & inter_logical == TRUE
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
}
halo_spatial$After = AfterLoop
halo_spatial$Before = BeforeLoop

table(halo_spatial$After)
table(halo_spatial$Before)

coor = st_coordinates(halo_spatial$geometry)

output = cbind(halo_spatial, coor)

output$geometry = NULL


write.csv(output, file = "D:/Documents/5872M-Dissertation/Data/Geometries/Halogen_2016_With_After_Before_15mins.csv",row.names=FALSE)

Speed = data.frame(Ave1 = halo_spatial$Average_Speed_Lane_1, Ave2 = halo_spatial$Average_Speed_Lane_2, Ave3 = halo_spatial$Average_Speed_Lane_3, Ave4 = halo_spatial$Average_Speed_Lane_4, Ave5 = halo_spatial$Average_Speed_Lane_5)
Occupancy = data.frame(Ave1 = halo_spatial$Occupancy_Lane_1, Ave2 = halo_spatial$Occupancy_Lane_2, Ave3 = halo_spatial$Average_Speed_Lane_3, Ave4 = halo_spatial$Average_Speed_Lane_4, Ave5 = halo_spatial$Average_Speed_Lane_5)
Headway = data.frame(Ave1 = halo_spatial$Average_Headway_Lane_1, Ave2 = halo_spatial$Average_Headway_Lane_2, Ave3 = halo_spatial$Average_Headway_Lane_3, Ave4 = halo_spatial$Average_Headway_Lane_4, Ave5 = halo_spatial$Average_Headway_Lane_5)



Speed[Speed$Ave1 == 255,] = 0
Speed[Speed$Ave2 == 255,] = 0
Speed[Speed$Ave3 == 255,] = 0
Speed[Speed$Ave4 == 255,] = 0
Speed[Speed$Ave5 == 255,] = 0

Occupancy[Occupancy$Ave1 == 255,] = 0
Occupancy[Occupancy$Ave2 == 255,] = 0
Occupancy[Occupancy$Ave3 == 255,] = 0
Occupancy[Occupancy$Ave4 == 255,] = 0
Occupancy[Occupancy$Ave5 == 255,] = 0

# Maybe

Headway[Headway$Ave1 == 255,] = 0
Headway[Headway$Ave2 == 255,] = 0
Headway[Headway$Ave3 == 255,] = 0
Headway[Headway$Ave4 == 255,] = 0
Headway[Headway$Ave5 == 255,] = 0

TotalFlow[TotalFlow$Ave1 == 255,] = 0
TotalFlow[TotalFlow$Ave2 == 255,] = 0
TotalFlow[TotalFlow$Ave3 == 255,] = 0
TotalFlow[TotalFlow$Ave4 == 255,] = 0
TotalFlow[TotalFlow$Ave5 == 255,] = 0


myrowmean <- function(x) {
  zero <- x==0
  if (all(zero)) 0 else mean(x[!zero])
}

AveSpeed = as.data.frame(apply(Speed,1,myrowmean))
colnames(AveSpeed) = c("AveSpeed")

halo_spatial$AveSpeed = AveSpeed$AveSpeed

ggplot(halo_spatial, aes(factor(After), AveSpeed)) + 
  geom_violin()

AveSpeed = rowMeans(Speed[Speed != 0 & Speed!= 255,])

Speed = data.frame(Ave1 = halo_spatial$`Average Speed Lane 1`, Ave2 = halo_spatial$`Average Speed Lane 2`, Ave3 = halo_spatial$`Average Speed Lane 3`, Ave4 = halo_spatial$`Average Speed Lane 4`, Ave5 = halo_spatial$`Average Speed Lane 5`)

AveSpeed = rowMeans(Speed[Speed != 0 & Speed!= 255])