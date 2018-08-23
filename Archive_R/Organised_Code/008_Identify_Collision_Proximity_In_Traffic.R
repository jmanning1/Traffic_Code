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

# Creates 1080 NA Datetimes
# halo_spatial$Datetime = ymd_hms(as.character(as.POSIXct(paste(paste0(halo_spatial$Year, "-", halo_spatial$Month, "-", halo_spatial$Day),paste0(halo_spatial$`Time GMT`, ":00")), format="%Y-%b-%d %H:%M:%S")))
# datetime_fail = halo_spatial[is.na(halo_spatial$Datetime) == TRUE,]
# write.csv(datetime_fail, file = "D:/Documents/5872M-Dissertation/Data/Potential_Data_Issues/DateTime_Fail.csv", row.names=FALSE)




halo_spatial$Datetime = ymd_hms(paste(paste0(halo_spatial$Year,"-",halo_spatial$Month,"-",halo_spatial$Day),
                                      paste0(halo_spatial$`Time GMT`, ":00")
                                      )
                                )


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


write.csv(output, file = "D:/Documents/5872M-Dissertation/Data/Geometries/Halogen_2016_With_After_Before_1hr.csv",row.names=FALSE)

# Calculate Averages

backup = halo_spatial

halo_spatial = output

colnames(halo_spatial) = c("Control_Office","Geographic_Address","Year",
                           "Month","Day","Day_of_Week",
                           "Type_of_Day","Days_After_Nearest_Bank_Holiday","Time_GMT",
                           "Number_of_Lanes","Flow_Category_1","Flow_Category_2",
                           "Flow_Category_3","Flow_Category_4","Average_Speed_Lane_1",
                           "Total_Flow_Lane_1","Occupancy_Lane_1","Average_Headway_Lane_1",
                           "Average_Speed_Lane_2","Total_Flow_Lane_2","Occupancy_Lane_2",
                           "Average_Headway_Lane_2","Average_Speed_Lane_3","Total_Flow_Lane_3",
                           "Occupancy_Lane_3","Average_Headway_Lane_3","Average_Speed_Lane_4",
                           "Total_Flow_Lane_4","Occupancy_Lane_4","Average_Headway_Lane_4",
                           "Average_Speed_Lane_5","Total_Flow_Lane_5","Occupancy_Lane_5",
                           "Average_Headway_Lane_5","Datetime",
                           "After","Before", "X", "Y")

Speed = data.frame(Ave1 = halo_spatial$Average_Speed_Lane_1,
                   Ave2 = halo_spatial$Average_Speed_Lane_2,
                   Ave3 = halo_spatial$Average_Speed_Lane_3,
                   Ave4 = halo_spatial$Average_Speed_Lane_4,
                   Ave5 = halo_spatial$Average_Speed_Lane_5)


Occupancy = data.frame(Ave1 = halo_spatial$Occupancy_Lane_1,
                       Ave2 = halo_spatial$Occupancy_Lane_2,
                       Ave3 = halo_spatial$Occupancy_Lane_3,
                       Ave4 = halo_spatial$Occupancy_Lane_4,
                       Ave5 = halo_spatial$Occupancy_Lane_5)

Headway = data.frame(Ave1 = halo_spatial$Average_Headway_Lane_1,
                     Ave2 = halo_spatial$Average_Headway_Lane_2,
                     Ave3 = halo_spatial$Average_Headway_Lane_3,
                     Ave4 = halo_spatial$Average_Headway_Lane_4,
                     Ave5 = halo_spatial$Average_Headway_Lane_5)

TotalFlow = data.frame(Ave1 = halo_spatial$Total_Flow_Lane_1,
                       Ave2 = halo_spatial$Total_Flow_Lane_2,
                       Ave3 = halo_spatial$Total_Flow_Lane_3,
                       Ave4 = halo_spatial$Total_Flow_Lane_4,
                       Ave5 = halo_spatial$Total_Flow_Lane_5)


Speed[Speed$Ave1 == 255, 1] = 0
Speed[Speed$Ave2 == 255, 2] = 0
Speed[Speed$Ave3 == 255, 3] = 0
Speed[Speed$Ave4 == 255, 4] = 0
Speed[Speed$Ave5 == 255, 5] = 0

Occupancy[Occupancy$Ave1 == 255, 1] = 0
Occupancy[Occupancy$Ave2 == 255, 2] = 0
Occupancy[Occupancy$Ave3 == 255, 3] = 0
Occupancy[Occupancy$Ave4 == 255, 4] = 0
Occupancy[Occupancy$Ave5 == 255, 5] = 0

Headway[Headway$Ave1 == 255, 1] = 0
Headway[Headway$Ave2 == 255, 2] = 0
Headway[Headway$Ave3 == 255, 3] = 0
Headway[Headway$Ave4 == 255, 4] = 0
Headway[Headway$Ave5 == 255, 5] = 0

TotalFlow[TotalFlow$Ave1 == 255, 1] = 0
TotalFlow[TotalFlow$Ave2 == 255, 2] = 0
TotalFlow[TotalFlow$Ave3 == 255, 3] = 0
TotalFlow[TotalFlow$Ave4 == 255, 4] = 0
TotalFlow[TotalFlow$Ave5 == 255, 5] = 0


myrowmean = function(x) {
  zero = x == 0
  if (all(zero)) {0} else {mean(x[!zero])}
}

AveSpeed = as.data.frame(apply(Speed,1,myrowmean))
colnames(AveSpeed) = c("AveSpeed")

AveOccupancy = as.data.frame(apply(Occupancy,1,myrowmean))
colnames(AveOccupancy) = c("AveOccupancy")

AveHeadway = as.data.frame(apply(Headway,1,myrowmean))
colnames(AveHeadway) = c("AveHeadway")

TotalFlow = as.data.frame(apply(TotalFlow,1,sum))
colnames(TotalFlow) = c("TotalFlow")

halo_spatial$AveSpeed = AveSpeed$AveSpeed
halo_spatial$AveOccupancy = AveOccupancy$AveOccupancy
halo_spatial$AveHeadway = AveHeadway$AveHeadway
halo_spatial$TotalFlow = TotalFlow$TotalFlow

# Remove Missing

nrow(halo_spatial[halo_spatial$AveSpeed == 0 & halo_spatial$AveOccupancy == 0 & halo_spatial$AveHeadway == 0 & halo_spatial$TotalFlow == 0, ])

Missing = halo_spatial[halo_spatial$AveSpeed == 0 & halo_spatial$AveOccupancy == 0 & halo_spatial$AveHeadway == 0 & halo_spatial$TotalFlow == 0, ]

'%!in%' <- function(x,y)!('%in%'(x,y))

all_missing = Missing[Missing$Geographic_Address %!in% unique(halo_spatial$Geographic_Address),]

missing_report = as.data.frame(unique(all_missing$Geographic_Address))

write.csv(missing_report, file = "D:/Documents/5872M-Dissertation/Data/Potential_Data_Issues/Potentially_Faulty_Sites.csv", row.names=FALSE)


halo_spatial = halo_spatial[!(halo_spatial$AveSpeed == 0 & halo_spatial$AveOccupancy == 0 & halo_spatial$AveHeadway == 0 & halo_spatial$TotalFlow == 0), ]


hist(halo_spatial$TotalFlow, breaks = max(halo_spatial$TotalFlow))
hist(halo_spatial$TotalFlow[halo_spatial$Before == 1], breaks = max(halo_spatial$TotalFlow))
hist(halo_spatial$TotalFlow[halo_spatial$After == 1], breaks = max(halo_spatial$TotalFlow))

unique_sites = osgb_sites[osgb_sites$`Geographic Address` %in% unique(halo_spatial$Geographic_Address),]

webs = tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_sites) +
  tm_dots() + 
  tm_layout(title = "Potential Webtris Locations", main.title.size = 10)


mids = tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) + 
  tm_shape(unique_sites) +
  tm_dots() +
  tm_layout(title = "MIDAS Gold Locations Near Collisions", main.title.size = 10)

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(webs, vp=viewport(layout.pos.col = 1))
print(mids, vp=viewport(layout.pos.col = 2))


no_0_Speed = halo_spatial[halo_spatial$AveSpeed != 0, ]
no_0_Occupancy = halo_spatial[halo_spatial$AveOccupancy != 0, ]
no_0_Headway = halo_spatial[halo_spatial$AveHeadway != 0, ]
no_0_TotalFlow = halo_spatial[halo_spatial$TotalFlow != 0, ]


table(no_0$After)

# Violins of results After

pl1 = ggplot(halo_spatial, aes(factor(After), AveSpeed)) + 
  geom_violin(aes(fill = factor(After)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("After Collision") + ylab("Average Speed") + theme(legend.position="none")

pl2 = ggplot(halo_spatial, aes(factor(After), AveOccupancy)) + 
  geom_violin(aes(fill = factor(After)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("After Collision") + ylab("Average Occupancy") + theme(legend.position="none")

pl3 = ggplot(halo_spatial, aes(factor(After), AveHeadway)) + 
  geom_violin(aes(fill = factor(After)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("After Collision") + ylab("Average Headway") + theme(legend.position="none")

pl4 = ggplot(halo_spatial, aes(factor(After), TotalFlow)) + 
  geom_violin(aes(fill = factor(After)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("After Collision") + ylab("Total Flow") + theme(legend.position="none")

# Violins of results Before

pl5 = ggplot(halo_spatial, aes(factor(Before), AveSpeed)) + 
  geom_violin(aes(fill = factor(Before)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("Before Collision") + ylab("Average Speed") + theme(legend.position="none")

pl6 = ggplot(halo_spatial, aes(factor(Before), AveOccupancy)) + 
  geom_violin(aes(fill = factor(Before)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("Before Collision") + ylab("Average Occupancy") + theme(legend.position="none")
  

pl7 = ggplot(halo_spatial, aes(factor(Before), AveHeadway)) + 
  geom_violin(aes(fill = factor(Before)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("Before Collision") + ylab("Average Headway") + theme(legend.position="none")

pl8 = ggplot(halo_spatial, aes(factor(Before), TotalFlow)) + 
  geom_violin(aes(fill = factor(Before)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("Before Collision") + ylab("Total Flow") + theme(legend.position="none")

grid.arrange(pl5, pl6, pl7, pl8,pl1, pl2, pl3, pl4, nrow = 2, ncol = 4, top = "Patterns in the Traffic Characteristics Around a Collision Compared to the Rest of the MIDAS Gold Dataset")


mean(halo_spatial$AveSpeed[halo_spatial$After == 0 & halo_spatial$AveSpeed != 0])
mean(halo_spatial$AveSpeed[halo_spatial$After == 1 & halo_spatial$AveSpeed != 0])
mean(halo_spatial$AveSpeed[halo_spatial$Before == 0 & halo_spatial$AveSpeed != 0])
mean(halo_spatial$AveSpeed[halo_spatial$Before == 1 & halo_spatial$AveSpeed != 0])

# Create Graphs of MIDAS Gold

# Speed

halo_spatial2 = st_as_sf(halo_spatial, coords = c("X", "Y"), crs = 27700)

halo_spatial2$Hour = hour(halo_spatial2$Datetime)

raster_template = raster(extent(150000, 650000, 0, 600000), resolution = 10000,
                         crs = st_crs(27700)$proj4string)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(0,1,2,3),], raster_template, 
                       field = "AveSpeed", fun=mean)

colours = palette(rev(brewer.pal(n = 7, name = "YlOrRd")))

a0= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0 ,20, 40, 60, 80, 100, 120, 140), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "00:00 - 03:59",
            main.title.size = 10,
            legend.show = FALSE)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(4,5,6,7),], raster_template, 
                       field = "AveSpeed", fun=mean)

a1= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0 ,20, 40, 60, 80, 100, 120, 140), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "4:00 - 7:59",
            main.title.size = 10,
            legend.show = FALSE)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(8,9,10,11),], raster_template, 
                       field = "AveSpeed", fun=mean)

a2= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0 ,20, 40, 60, 80, 100, 120, 140), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "8:00 - 11:59",
            main.title.size = 10,
            legend.show = FALSE)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(12,13,14,15),], raster_template, 
                       field = "AveSpeed", fun=mean)

a3= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0 ,20, 40, 60, 80, 100, 120, 140), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "12:00 - 15:59",
            main.title.size = 10,
            legend.show = FALSE, attr.outside = TRUE)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(16,17,18,19),], raster_template, 
                       field = "AveSpeed", fun=mean)

a4= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0 ,20, 40, 60, 80, 100, 120, 140), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "16:00 - 19:59",
            main.title.size = 10,
            legend.show = FALSE)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(20,21,22,23),], raster_template, 
                       field = "AveSpeed", fun=mean)

a5= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0 ,20, 40, 60, 80, 100, 120, 140), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "20:00 - 23:59",
            main.title.size = 10,
            legend.show = FALSE)

leg = tm_shape(col_raster) +
  tm_raster(title = "Average Speed", breaks = c(0 ,20, 40, 60, 80, 100, 120, 140), palette = colours, legend.is.portrait = TRUE) +
  tm_layout(legend.only = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 1,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow = 3, ncol = 4, heights = unit(c(0.5, 6, 6), "null"), widths = unit(c(4,4,4,2), "null"))))
grid.text("Average Speed of MIDAS Gold Dataset over 4 Hour Periods", vp = viewport(layout.pos.row = 1, layout.pos.col = 1:3))
print(a0, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(a1, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(a2, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))
print(a3, vp=viewport(layout.pos.row = 3, layout.pos.col = 1))
print(a4, vp=viewport(layout.pos.row = 3, layout.pos.col = 2))
print(a5, vp=viewport(layout.pos.row = 3, layout.pos.col = 3))
print(leg, vp=viewport(layout.pos.row = 2, layout.pos.col = 4))

## Total Flow

colours = palette(brewer.pal(n = 9, name = "Blues"))
colours[1] = "#DDDDDD"

raster_template = raster(extent(150000, 650000, 0, 600000), resolution = 10000,
                         crs = st_crs(27700)$proj4string)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(0,1,2,3),], raster_template, 
                       field = "TotalFlow", fun=mean)

a0= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "00:00 - 03:59",
            main.title.size = 10,
            legend.show = FALSE)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(4,5,6,7),], raster_template, 
                       field = "TotalFlow", fun=mean)

a1= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "4:00 - 7:59",
            main.title.size = 10,
            legend.show = FALSE)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(8,9,10,11),], raster_template, 
                       field = "TotalFlow", fun=mean)

a2= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "8:00 - 11:59",
            main.title.size = 10,
            legend.show = FALSE)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(12,13,14,15),], raster_template, 
                       field = "TotalFlow", fun=mean)

a3= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "12:00 - 15:59",
            main.title.size = 10,
            legend.show = FALSE, attr.outside = TRUE)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(16,17,18,19),], raster_template, 
                       field = "TotalFlow", fun=mean)

a4= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "16:00 - 19:59",
            main.title.size = 10,
            legend.show = FALSE)

col_raster = rasterize(halo_spatial2[halo_spatial2$Hour %in% c(20,21,22,23),], raster_template, 
                       field = "TotalFlow", fun=mean)

a5= tm_shape(col_raster) +
  tm_raster(title = "", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf), palette = colours) +
  tm_shape(uk, bbox = st_bbox(c(xmin = 150000, xmax = 650000, ymax = 600000, ymin = 0), crs = st_crs(27700))) +
  tm_polygons(alpha = 0.1) +
  tm_shape(osgb_roads) +
  tm_lines(col = 'black', alpha = 0.2) + 
  tm_layout(title = "20:00 - 23:59",
            main.title.size = 10,
            legend.show = FALSE)

leg = tm_shape(col_raster) +
  tm_raster(title = "Total Flow", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf), palette = colours, legend.is.portrait = TRUE) +
  tm_layout(legend.only = TRUE,
            legend.title.size = 1.2,
            legend.text.size = 1,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow = 3, ncol = 4, heights = unit(c(0.5, 6, 6), "null"), widths = unit(c(4,4,4,2), "null"))))
grid.text("Average Total Flow of MIDAS Gold Dataset over 4 Hour Periods", vp = viewport(layout.pos.row = 1, layout.pos.col = 1:3))
print(a0, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(a1, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(a2, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))
print(a3, vp=viewport(layout.pos.row = 3, layout.pos.col = 1))
print(a4, vp=viewport(layout.pos.row = 3, layout.pos.col = 2))
print(a5, vp=viewport(layout.pos.row = 3, layout.pos.col = 3))
print(leg, vp=viewport(layout.pos.row = 2, layout.pos.col = 4))









