library(RCurl)

# Important - Remove Rapid Login Before Sharing
# Code Adapted from https://hydroecology.net/downloading-lots-of-files-with-r/

# Empties Destination Folder
data_dir = "tcd-data"
dir.create(data_dir)

# CO = c("10", "20", "30", "40", "50", "60", "70", "79")
CO = c("10")
year = c("2015", "2016", "2017", "2018")
month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_2015 = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month_2018 = c("Jan", "Feb", "Mar", "Apr", "May", "Jun")

# Secure Login
# un = readline("Type the username:")
# pw = readline("Type the password:")

for(a in seq(length(CO))){
  
  for(b in seq(length(year))){
    if(year[b] == "2015"){
      month_current = month_2015
    } else if (year[b] == "2018"){
      month_current = month_2018
    } else {
      month_current = month}
    
    for(d in seq(length(month_current))){
      
# Set Co, Year and Month to Download
      baseurl = paste0("https://www.midas-data.org.uk/midasdata/Trafdata/Co", CO[a], "/", year[b], "/",month_current[d], "/")
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
      mydir = "tcd-data/"
      
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
}

# unzip to tcd subfolder
dir.create(file.path(data_dir, "tcd"))
f_bz2 = list.files(path = data_dir, pattern = "*.bz2$", full.names = TRUE)
f_tcd = gsub(pattern = ".bz2", replacement = "", x = f_bz2)
for(i in 1:length(f_bz2)) {
  R.utils::bunzip2(f_bz2[i], f_tcd[i])
}
dir.create("tcd")
file.copy(f_tcd[2:length(f_tcd)], "tcd")
