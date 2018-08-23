library(RCurl)

# Important - Remove Rapid Login Before Sharing
# Code Adapted from https://hydroecology.net/downloading-lots-of-files-with-r/

# Empties Destination Folder
unlink("D:/Documents/5872M-Dissertation/Data/Original/Auto/*.tcd")

CO = c("10", "20", "30", "40", "50", "60", "70", "79")
year = c("2015", "2016", "2017", "2018")
month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_2015 = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_2018 = c("Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Set Co, Year and Month to Download
baseurl = "https://www.midas-data.org.uk/midasdata/Trafdata/Co10/2016/Apr/"

# Secure Login
un = readline("Type the username:")
pw = readline("Type the password:")

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
mydir = "D:/Documents/5872M-Dissertation/Data/Original/Auto/"

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

