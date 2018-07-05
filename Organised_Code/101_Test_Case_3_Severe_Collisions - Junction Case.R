# Extract of Test sites from Halogen Data - Junction Accident - No Effect

# Run 007_Formatting_Halogen_Data on 09/01/2016

# Libraries Used

# library(ggplot2)
# library(gridExtra)

# Accident Profile M27/9188A - Severity 2 - Upstream

TC2TU = halo_spatial[halo_spatial$`Geographic Address` == "M27/9188A",]
TC2buffer = st_buffer(TC2TU, 500)
TC2A = stat19_date[TC2buffer,]

# Speed

USL1 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 1`), colour = "blue") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 170))
USL2 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 2`), colour = "red") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
USL3 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 3`), colour = "green") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
USL4 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 4`), colour = "purple") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))

# Occupancy

UOL1 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 1`), colour = "blue") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 100))
UOL2 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 2`), colour = "red") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))
UOL3 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 3`), colour = "green") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))
UOL4 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 4`), colour = "purple") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))

# Headway

UHL1 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 1`), colour = "blue") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 260))
UHL2 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 2`), colour = "red") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
UHL3 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 3`), colour = "green") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
UHL4 = ggplot(data = TC2TU, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 4`), colour = "purple") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))


grid.arrange(USL1, USL2, USL3, USL4,UOL1, UOL2, UOL3, UOL4, UHL1, UHL2, UHL3, UHL4, nrow = 3, ncol = 4, top = "M27/9188A Upstream of Collision")

# Accident Profile M27/9188B - Severity 2 - Rubbernecking Dowstream

TC2TRD = halo_spatial[halo_spatial$`Geographic Address` == "M27/9188B",]
TC2buffer = st_buffer(TC2TRD, 500)
TC2A = stat19_date[TC2buffer,]

# Speed

RSL1 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 1`), colour = "blue") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 170))
RSL2 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 2`), colour = "red") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
RSL3 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 3`), colour = "green") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))
RSL4 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Speed Lane 4`), colour = "purple") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 170))

# Occupancy

ROL1 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 1`), colour = "blue") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 100))
ROL2 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 2`), colour = "red") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))
ROL3 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 3`), colour = "green") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))
ROL4 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Occupancy Lane 4`), colour = "purple") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 100))

# Headway

RHL1 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 1`), colour = "blue") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2) +
  ylim(c(0, 260))
RHL2 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 2`), colour = "red") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
RHL3 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 3`), colour = "green") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))
RHL4 = ggplot(data = TC2TRD, aes(x = `Time GMT`)) +
  geom_point(mapping = aes(y = `Average Headway Lane 4`), colour = "purple") +
  geom_point(data = TC2A, mapping = aes(x = `Time`,y = 50), size = 2)  +
  ylim(c(0, 260))

grid.arrange(RSL1, RSL2, RSL3, RSL4,ROL1, ROL2, ROL3, ROL4, RHL1, RHL2, RHL3, RHL4, nrow = 3, ncol = 4, top = "M27/9188B 'Rubbernecking Effect' Downstream of Collision")
