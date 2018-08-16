# Download Halogen Data in 2016 from midas Site - Takes a few hours to run dependant on Internet Speed.
# Only run if don't have a local copy of the Halogen Data.

# Inputs:
#   Username and Password
#   Edit Inputs of CO, year and month as nessarsary to extract wanted info.

# Outputs:
#   tcd.bz2 files of MIDAS Data, seperated by CO and date
#   Run extract_tcd_Auto_to_tcd_only Batch file to turn tcd.bz2 into tcd
#   Warning - Output of 2016 around 75Gb in tcd format


# library(RCurl)

# Important - Remove Rapid Login Before Sharing
# Code Adapted from https://hydroecology.net/downloading-lots-of-files-with-r/

# Empties Destination Folder
unlink("Data/Original/Auto/*.tcd")

CO = c("10", "20", "30", "40", "50", "60", "70", "79")
year = c("2015", "2016", "2017", "2018")
# year = "2016"
month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# month = "Dec"

# Secure Login
un = readline("Type the username:")
pw = readline("Type the password:")

for(a in seq(length(CO))){
  month_current = month
  for(d in seq(length(month_current))){
    
    # Set Co, Year and Month to Download
    baseurl = paste0("https://www.midas-data.org.uk/midasdata/Trafdata/Co", CO[a], "/", "2016", "/", month_current[d], "/")
    print(baseurl)
    
    # create username password in one variable
    upw = paste(un, pw, sep = ":")
    
    # access webpage as single string
    webpage = getURL(baseurl, userpwd = upw)
    
    # Parse webpage into multiple lines
    tc = textConnection(webpage)
    contents = readLines(tc)
    close(tc)
    
    # extract .tcd.bz2 file links
    rx = gregexpr("(?<=<a href=\")([0-9]{8})(.tcd.bz2)(?=\">)",
                  contents, perl = TRUE)
    urls = unlist(regmatches(contents, rx))
    urls
    
    # To Download ###########################################
    
    # Set Output Directory
    mydir = "Data/Original/Auto/"
    
    # Generate Full Urls
    fileurls = paste0(baseurl, urls)
    
    # generate the destination file paths
    filepaths = file.path(mydir, urls)
    
    # create a container for loop outputs
    res = vector("list", length = length(filepaths))
    
    # loop through urls and download
    for(i in seq(length(fileurls)))
      writeBin(getBinaryURL(fileurls[i], userpwd = upw),
               con = filepaths[i])
  }
}

# read these, e.g. with (failing):
# sites = unique(osgb_sites$`Geographic Address`)
# RoadData(startDate = "2016-12-01", endDate = "2016-12-30", tcdFileRoot = "Data/Original/Auto/", sites)

