# Extract of Test sites from Halogen Data

library(oneminutetrafficdata)
library(sf)
library(dplyr)
library(data.table)
library(plyr)

stat19 = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Stat19_2016_2km_Subset.csv")))

stat19_spatial = st_as_sf(stat19, coords = c("X", "Y"), crs = 27700)

class(stat19_spatial) # Should be:  "sf"         "data.frame"

# Run up to the 500m in Subset_Stat19_By_Proximity_to_site_500m

  Sites_accidents = unique(osgb_sites$`Geographic Address`)
  dates = as.data.frame(unique(stat19_spatial$Date))
  colnames(dates) = "Date"
  dates_formatted = as.Date(dates[,1], "%d/%m/%Y")
  jan = dates_formatted[months(dates_formatted) == "February"]
  jan= as.character(jan)
  #completed_cases = c("2016-01-11","2016-01-18", "2016-01-19", "2016-01-23","2016-01-28")
  #'%!in%' <- function(x,y)!('%in%'(x,y))
  #jan = jan[jan %!in% completed_cases]
# dates = c("2016-01-01", "2016-01-02")
# j = 1
 
# get rid of slip roads

Site_Accidents_reduce = Sites_accidents[!grepl("K", Sites_accidents) & !grepl("J", Sites_accidents) & !grepl("L", Sites_accidents) & !endsWith("M", Sites_accidents)]
  
  
length(jan)
for(j in seq(length(jan))){
  

rd = RoadData(startDate = jan[j], endDate = jan[j], tcdFileRoot = "D:/Documents/5872M-Dissertation/Data/Halogen_2016",Site_Accidents_reduce)
#rd = RoadData(startDate = jan[j], endDate = jan[j], tcdFileRoot = "D:/Documents/5872M-Dissertation/Data/Halogen_2016", "A1M/2259A", "A1M/2259B")
#2016-12-31

# Make Dataframe

df_list = lapply(seq(length(rd)),function(i){
  df = as.data.frame(rd[i], stringsAsFactors = FALSE)
})
head(do.call(bind_rows,df_list))

df1 = as.data.frame(data.table::rbindlist(df_list, use.names=TRUE, fill=TRUE))

# 
# for(i in seq(length(rd))){
#   if(i == 1){
#     df1 = as.data.frame(rd[i])
#   }else{
#     df = as.data.frame(rd[i])
#     df1 = bind_rows(df1, df)
#   }
# }

csv_file = paste0("D:/Documents/5872M-Dissertation/Data/Halogen_Site_csv/", jan[j], ".csv")

# Output to csv

write.csv(df1, file = csv_file, row.names=FALSE)
rm(df1)
rm(df_list)
rm(rd)
gc()
}
?df





# mkRow <- function(nCol) {
#   x <- as.list(rnorm(nCol))
#   # make row mixed types by changing first column to string
#   x[[1]] <- ifelse(x[[1]]>0,'pos','neg')
#   names(x) <- paste('x',seq_len(nCol),sep='.')
#   x
# }
# 
# mkFrameInPlace <- function(nRow,nCol,classHack=TRUE) {
#   r1 <- mkRow(nCol)
#   d <- data.frame(r1,
#                   stringsAsFactors=FALSE)
#   if(nRow>1) {
#     d <- d[rep.int(1,nRow),]
#     if(classHack) {
#       # lose data.frame class for a while
#       # changes what S3 methods implement
#       # assignment.
#       d <- as.list(d) 
#     }
#     for(i in seq.int(2,nRow,1)) {
#       ri <- mkRow(nCol)
#       for(j in seq_len(nCol)) {
#         d[[j]][i] <- ri[[j]]
#       }
#     }
#   }
#   if(classHack) {
#     d <- data.frame(d,stringsAsFactors=FALSE)
#   }
#   d
# }
