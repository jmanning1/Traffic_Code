# Load files

# for march

# temp = list.files(path ="D:/Documents/5872M-Dissertation/Data/Halogen_Site_Severe/", pattern="*.csv", full.names = TRUE)

# for 3 day window dataset 500m

temp = list.files(path ="D:/Documents/5872M-Dissertation/Data/Halogen_Severe_window/", pattern="*.csv", full.names = TRUE)


# for 3 day window dataset 2km

# temp = list.files(path ="D:/Documents/5872M-Dissertation/Data/Halogen_Severe_window_2km/", pattern="*.csv", full.names = TRUE)


#myfiles = lapply(temp, fread)

myfiles = lapply(temp, read.csv, sep = ",")

# Make Dataframe
  
halo = as.data.frame(data.table::rbindlist(myfiles, use.names=TRUE, fill=TRUE))

rm(myfiles) # Huge Memory Usage 
