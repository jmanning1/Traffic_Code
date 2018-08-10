# Extract of Test sites from Halogen Data

# Run 007_Formatting_Halogen_Data on 28/01/2016

# Libraries Used

# library(ggplot2)
# library(gridExtra)

jan28 = as.data.frame(halo_spatial[date(halo_spatial$Datetime) == "2016-01-28",])
head(jan28)
colnames(jan28) = c("Control Office","Geographic Address",
                   "Year","Month",
                   "Day","Day of Week",
                   "Type of Day","Days After Nearest Bank Holiday",
                   "Time GMT","Number of Lanes",
                   "Flow Category 1","Flow Category 2",
                   "Flow Category 3","Flow Category 4",
                   "Average Speed Lane 1","Total Flow Lane 1",
                   "Occupancy Lane 1","Average Headway Lane 1",
                   "Average Speed Lane 2","Total Flow Lane 2",
                   "Occupancy Lane 2","Average Headway Lane 2",
                   "Average Speed Lane 3","Total Flow Lane 3",
                   "Occupancy Lane 3","Average Headway Lane 3",
                   "Average Speed Lane 4","Total Flow Lane 4",
                   "Occupancy Lane 4","Average Headway Lane 4",
                   "Average Speed Lane 5","Total Flow Lane 5",
                   "Occupancy Lane 5","Average Headway Lane 5", 
                   "After", "Before",
                   "X", "Y",
                   "AveSpeed","AveOccupancy", 
                   "AveHeadway", "AveTotalFlow")

jan28 = st_as_sf(jan28, coords = c("X", "Y"), crs = 27700)

stat19_date = stat19[stat19$Date == "2016-01-28" & stat19$Accident_Severity %in% c(1,2) & stat19$Junction_Detail == 0 & stat19$Road_Type == 3,]
stat19_date = st_as_sf(stat19_date, coords = c("X", "Y"), crs = 27700)


# Logistic Graph

logisitic = function(x) 1/(1+exp(-x))
X = seq(-20, 20, length.out = 100)
Y = logisitic(X)
plot(X, Y, xlab="Time", ylab="", xaxt='n', yaxt= 'n', cex.lab=2, cex.main=2, main = "Has a Collision Occurred?") + 
  lines(X,Y) + 
  axis(side = 2, at = c(0,1), labels = c("False", "True"), cex.axis=2)

# Reason for Random Forests

coll_sim = function(x) {0+1*(x > 0.10 & x < 0.20 | x > 0.65 & x < 0.95)}
X = seq(0,1, length.out=100)
graph = coll_sim(X)
Y = as.numeric(runif(100)<graph)

plot(X,-Y, xlab="Time", ylab="", xaxt='n', yaxt= 'n', cex.lab=2, cex.main=2, main = "Has a Collision Occurred Recently and Is Traffic Impacted?") + 
  lines(X,-Y) + 
  axis(side = 2, at = c(0,-1), labels = c("False", "True"), cex.axis=2)


# Accident Profile M1/2516B - Severity 2 - Downstream

TC1TD = jan28[jan28$`Geographic Address` == "M1/2516B",]
TC1buffer = st_buffer(TC1TD, 500)
TC1A = stat19_date[stat19_date$Accident_Index == "2016406CA0222",]

TC1A = stat19_date[TC1buffer,]

# Speed

DSL1 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 1`), colour = "blue") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 170))
DSL2 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 2`), colour = "red") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
DSL3 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 3`), colour = "green") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
DSL4 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 4`), colour = "purple") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))

# Occupancy

DOL1 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 1`), colour = "blue") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 100))
DOL2 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 2`), colour = "red") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))
DOL3 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 3`), colour = "green") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))
DOL4 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 4`), colour = "purple") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))

# Headway

DHL1 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 1`), colour = "blue") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 260))
DHL2 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 2`), colour = "red") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
DHL3 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 3`), colour = "green") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
DHL4 = ggplot(data = TC1TD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 4`), colour = "purple") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))


grid.arrange(DSL1, DSL2, DSL3, DSL4,DOL1, DOL2, DOL3, DOL4, DHL1, DHL2, DHL3, DHL4, nrow = 3, ncol = 4, top = "M1/2516B Downstream of Collision")

# Accident Profile M1/2519B - Severity 2 - Upstream

TC1TU = jan28[jan28$`Geographic Address` == "M1/2519B",]
TC1buffer = st_buffer(TC1TU, 500)
TC1A = stat19_date[stat19_date$Accident_Index == "2016406CA0222",]

TC1A = stat19_date[TC1buffer,]

# Speed

USL1 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 1`), colour = "blue") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 170))
USL2 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 2`), colour = "red") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
USL3 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 3`), colour = "green") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
USL4 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 4`), colour = "purple") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))

# Occupancy

UOL1 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 1`), colour = "blue") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 100))
UOL2 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 2`), colour = "red") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))
UOL3 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 3`), colour = "green") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))
UOL4 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 4`), colour = "purple") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))

# Headway

UHL1 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 1`), colour = "blue") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 260))
UHL2 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 2`), colour = "red") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
UHL3 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 3`), colour = "green") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
UHL4 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 4`), colour = "purple") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))

# Total Flow

UHL1 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 1`), colour = "blue") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 260))
UHL2 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 2`), colour = "red") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
UHL3 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 3`), colour = "green") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
UHL4 = ggplot(data = TC1TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Total Flow Lane 4`), colour = "purple") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))


grid.arrange(USL1, USL2, USL3, USL4,UOL1, UOL2, UOL3, UOL4, UHL1, UHL2, UHL3, UHL4, nrow = 3, ncol = 4, top = "M1/2519B Upstream of Collision")

# Accident Profile eg3 - Severity 2 - Rubbernecking

TC1TR = jan28[jan28$`Geographic Address` == "M1/2516A",]
TC1buffer = st_buffer(TC1TR, 500)
TC1A = stat19_date[TC1buffer,]

# Speed

RSL1 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 1`), colour = "blue") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 170))
RSL2 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 2`), colour = "red") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
RSL3 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 3`), colour = "green") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
RSL4 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 4`), colour = "purple") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))

# Occupancy

ROL1 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 1`), colour = "blue") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 100))
ROL2 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 2`), colour = "red") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))
ROL3 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 3`), colour = "green") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))
ROL4 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 4`), colour = "purple") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))

# Headway

RHL1 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 1`), colour = "blue") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 260))
RHL2 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 2`), colour = "red") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
RHL3 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 3`), colour = "green") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
RHL4 = ggplot(data = TC1TR, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 4`), colour = "purple") +
  geom_point(data = TC1A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))


grid.arrange(RSL1, RSL2, RSL3, RSL4,ROL1, ROL2, ROL3, ROL4, RHL1, RHL2, RHL3, RHL4, nrow = 3, ncol = 4, top = "M1/2516A 'Rubbernecking Effect' of Collision")
