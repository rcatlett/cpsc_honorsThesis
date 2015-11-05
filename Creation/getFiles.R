require("RCurl")
source("formatFiles.R")
source("getNBA.R")

years <- c('88-89', '89-90', '90-91', '91-92', '92-93', '93-94', '94-95', '95-96', 
           '96-97', '97-98', '98-99', '99-00', '00-01', '01-02', '02-03', '03-04', 
           '04-05', '05-06', '06-07', '07-08', '08-09', '09-10', '10-11', '11-12', 
           '12-13', '13-14')
filenames <- c()
for (year in years){
  site <- paste("http://www.dougstats.com/",year,"RD.html", sep="")
  if (!url.exists(site)){
    site <- paste("http://www.dougstats.com/",year,"RD.txt", sep="")
  }
  files <- c(files,file)
  file <- paste("y", year, ".csv", sep="")
  download.file(site, file, quiet=TRUE)  
}

formatfiles(filenames)
NBA <- data.frame()
NBA <- getNBA(filenames, NBA)