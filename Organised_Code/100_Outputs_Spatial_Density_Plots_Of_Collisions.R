# Libraries Used:

#library(spatstat)
# ibrary(sp)

# Run 003 first

# All Collisions within 2km of Traffic Counter

stat19 = output

# Severe Collisions

stat19 = output[output$Accident_Severity == 2,]

# Fatal Collisions

stat19 = output[output$Accident_Severity == 1,]

# Severe and Fatal Collisions

stat19 = output[output$Accident_Severity %in% c(1,2),]

o = owin(xrange = range(stat19$Location_Easting_OSGR), range(stat19$Location_Northing_OSGR))
ac_pp = spatstat::ppp(x = stat19$Location_Easting_OSGR, y = stat19$Location_Northing_OSGR, window = o)
Dens <- density(ac_pp, adjust = 0.05, diggle = TRUE, at="pixels", kernel = "gaussian")  # create density object Info: https://www.rdocumentation.org/packages/spatstat/versions/1.56-0/topics/density.ppp

class(Dens)  # just for interest: it's got it's of pixel image class

# Plot
# All Collisions within 2km of Traffic Counter

plot(Dens, main="All Collisions Within 2km of a Traffic Count Site") # Number of points per pixel area

# Severe Collisions

plot(Dens, main="Severe Collisions Within 2km of a Traffic Count Site") # Number of points per pixel area

# Fatal Collisions

plot(Dens, main="Fatal Collisions Within 2km of a Traffic Count Site") # Number of points per pixel area

# Severe and Fatal Collisions

plot(Dens, main="Severe and Fatal Collisions Within 2km of a Traffic Count Site") # Number of points per pixel area

dim(Dens)
