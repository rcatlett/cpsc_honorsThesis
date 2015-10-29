require("plyr")

getYear <- function(fileName, data) {
  information <- read.csv(fileName, sep="", stringsAsFactor=FALSE)
  data <- rbind.fill(data, information)
  return (data)
}

filenames <- Sys.glob("y*.csv")
NBA <- data.frame()
for (file in filenames){
  NBA <- getYear(file, NBA)
}


