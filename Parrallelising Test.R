library(parallel)
library(foreach)
library(doParallel)
library(oneminutetrafficdata)

no_cores <- detectCores()
no_cores

base <- 4

registerDoParallel(makeCluster(no_cores))

dates = c("2016-01-01", "2016-01-02")

for(j in seq(length(dates))){

foreach(dates = c("2016-01-01", "2016-01-02"), .combine = , .packages="oneminutetrafficdata")  %dopar% {
  rd = RoadData(startDate = dates, endDate = dates, tcdFileRoot = "D:/Documents/5872M-Dissertation/Data/Halogen_2016", "A1M/2259B", "A1M/2259A")
}
  for(i in seq(length(rd))){
    if(i == 1){
      df1 = as.data.frame(rd[i])
    }else{
      df = as.data.frame(rd[i])
      df1 = rbind(df1, df)
    }
  
  }
}
  csv_file = paste0("D:/Documents/5872M-Dissertation/Data/Halogen_Site_csv/", dates, ".csv")
}

?foreach
