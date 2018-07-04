


# Libraries Used:

#library(spatstat)
# ibrary(sp)

#ac_sp = as(osgb_joined, "Spatial")
#sSp <- as(sp::SpatialPoints(ac_sp), "ppp")  # convert points to pp class
o = owin(xrange = range(osgb_joined$Location_Easting_OSGR), range(osgb_joined$Location_Northing_OSGR))
ac_pp = spatstat::ppp(x = osgb_joined$Location_Easting_OSGR, y = osgb_joined$Location_Northing_OSGR, window = o)
Dens <- density(ac_pp, adjust = 0.05, diggle = TRUE, at="pixels", kernel = "gaussian")  # create density object Info: https://www.rdocumentation.org/packages/spatstat/versions/1.56-0/topics/density.ppp

class(Dens)  # just for interest: it's got it's of pixel image class
plot(Dens) # Number of points per pixel area

dim(Dens)
