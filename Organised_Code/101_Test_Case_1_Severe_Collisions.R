# Extract of Test sites from Halogen Data

# Run 007_Formatting_Halogen_Data on 28/01/2016

# Libraries Used

# library(ggplot2)
# library(gridExtra)

# Accident Profile M1/2516B - Severity 2 - Downstream

TC1TD = halo_spatial[halo_spatial$`Geographic Address` == "M1/2516B",]
TC1buffer = st_buffer(TC1TD, 500)
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

TC1TU = halo_spatial[halo_spatial$`Geographic Address` == "M1/2519B",]
TC1buffer = st_buffer(TC1TU, 500)
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


grid.arrange(USL1, USL2, USL3, USL4,UOL1, UOL2, UOL3, UOL4, UHL1, UHL2, UHL3, UHL4, nrow = 3, ncol = 4, top = "M1/2519B Upstream of Collision")

# Accident Profile eg3 - Severity 2 - Rubbernecking

TC1TR = halo_spatial[halo_spatial$`Geographic Address` == "M1/2516A",]
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
