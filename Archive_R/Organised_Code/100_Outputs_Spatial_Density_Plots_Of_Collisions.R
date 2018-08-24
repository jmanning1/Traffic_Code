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

# Missed Collisions

stat19 = outside_stat19

o = owin(xrange = range(stat19$Location_Easting_OSGR), range(stat19$Location_Northing_OSGR))
# o = owin(xrange = c(0,700000), c(0,700000))
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

plot(Dens, main="Collisions Farther than 2km from a Traffic Count Site") # Number of points per pixel area

dim(Dens)

AveDens = table(cut(stat19$Location_Easting_OSGR, 50), cut(stat19$Location_Northing_OSGR,50))
image2D(z = AveDens, main="All Collisions Within 2km of a Traffic Count Site") + plot()
image2D(z = AveDens, main="Severe Collisions Within 2km of a Traffic Count Site")
image2D(z = AveDens, main="Fatal Collisions Within 2km of a Traffic Count Site")
image2D(z = AveDens, main="Severe and Fatal Collisions Within 2km of a Traffic Count Site")
image2D(z = AveDens, main="Collisions Farther than 2km from a Traffic Count Site")

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
Y = coll_sim(X)

plot(X,-Y, xlab="Time", ylab="", xaxt='n', yaxt= 'n', cex.lab=2, cex.main=2, main = "Has a Collision Occurred Recently and Is Traffic Impacted?") + 
  lines(X,-Y) + 
  axis(side = 2, at = c(0,-1), labels = c("False", "True"), cex.axis=2)



