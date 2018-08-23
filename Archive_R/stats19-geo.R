# aim: analyse stats19 data to find crash clusters

ac_osgb = st_transform(ac_lds, crs = 27700)
# after running Stat19_2016.R ...
plot(ac_sf$geometry)
# mapview::mapview(ac_sf[1:900, ]) # visualisation method

# create buffers around MIDAS sites
buffer = st_buffer(midas_osgb, 500)
# subset to return only crashes within a distance (500m?) from MIDAS data points
ac_buffer = ac_sf[buffer, ]

# find which site has had the most crashes
ac_joined = st_join(ac_buffer, midas_osgb["site_name"])
summary(ac_joined$site_name) # top ones of interest

# time series of top site
# temporal clustering

# Alternative - spatial clusting approach: 
# plot(ac_lds)
# 
# library(spatstat)
# library(sp)
# 
# ac_sp = as(ac_lds, "Spatial")
# sSp <- as(sp::SpatialPoints(ac_sp), "ppp")  # convert points to pp class
# o = owin(xrange = range(ac_lds$Location_Easting_OSGR), range(ac_lds$Location_Northing_OSGR))
# ac_pp = spatstat::ppp(x = ac_lds$Location_Easting_OSGR, y = ac_lds$Location_Northing_OSGR, window = o)
# Dens <- density(ac_pp, adjust = 0.02)  # create density object
# class(Dens)  # just for interest: it's got it's of pixel image class
# plot(Dens)
