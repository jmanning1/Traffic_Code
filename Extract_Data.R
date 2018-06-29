library(ukboundaries)
library(stplanr)
library(sf)


###############
# Function Adaptation - Works

dl_stats19_2016 <- function(zip_url = paste0("http://data.dft.gov.uk/road-accidents-safety-data/",
                                        "dftRoadSafety_Accidents_2016.zip"), data_dir = tempdir()){
  
  # download and unzip the data if it's not present
  if(!"dftRoadSafety_Accidents_2016.csv" %in% list.files(data_dir)){
    destfile <- file.path(data_dir, "dftRoadSafety_Accidents_2016.zip")
    download.file(zip_url, destfile)
    unzip(destfile, exdir = data_dir)
  }
  
  print(paste0("Data saved at: ", list.files(data_dir,
                                             pattern = "csv", full.names = TRUE)))
  
}

###############

dl_stats19_2016()

###############
# Function Adaptation - Not Working

read_stats19_ac_2016 <- function(data_dir = tempdir(), filename = "dftRoadSafety_Accidents_2016.csv"){
  if(!filename %in% list.files(data_dir)){
    dl_stats19_2016()
  }
  
  # read the data in
  ac <- read.csv(file.path(data_dir, "dftRoadSafety_Accidents_2016.csv"))
  #   ve <- readr::read_csv(file.path(data_dir, "Vehicles0514.csv"))
  #   ca <- readr::read_csv(file.path(data_dir, "Casualties0514.csv"))
  
  # format ac data
  ac <- format_stats19_ac(ac)
  
  ac
  
}

a = read_stats19_ac_2016()

names(a)
