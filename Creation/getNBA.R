require('dplyr')
require("plyr")

# Create the data frame
filenames <- Sys.glob("../BBall_Data/y*.csv")
NBA <- data.frame()
for (file in filenames){
  # Reads each file - already formatted with headers
  information <- read.table(file, sep="", stringsAsFactor=FALSE, header=TRUE)
  NBA <- rbind.fill(NBA, information)
}

#Formatting the data
year <- ""
for (i in 1:nrow(NBA)) {
  #Fix the Year column from /##-## to ##-##
  year <- strsplit(NBA[i,1], "")[[1]]
  if (year[1] == "/") {
    year <- year[-1]
  }
  year <- paste(year, sep="", collapse = "")
  NBA[i,1] <- year
  
  #Capitalizes the Name for better printing
  name <- strsplit(NBA[i,2], ",")[[1]]
  NBA[i,2] <- paste(toupper(substring(name, 1, 1)), substring(name, 2), sep="", collapse=", ")
}

#Fix NAs in Team and Position
NBA$Team <- gsub("[^A-Z,a-z]", NA, NBA$Team)
NBA$PS <- gsub("[^A-Z,a-z]", NA, NBA$PS)

#Changes necessary columns to doubles instead of strings
for (i in 5:ncol(NBA)) {
  as.double(NBA[,i]) -> NBA[,i]
}

#Adds Efficiency rating for each player
# EFF = (PTS + REB + AST + STL + BLK − Missed FG − Missed FT − TO) / GP
for (i in 1:nrow(NBA)) {
  eff <- as.double((NBA[i,"PTS"] + NBA[i,"TR"] + NBA[i,"AS"] + NBA[i,"ST"] + NBA[i,"BK"] - (NBA[i, "FGA"]-NBA[i, "FGM"]) - (NBA[i,"FTA"]- NBA[i,"FTM"]) - NBA[i,"TO"])/NBA[i,"GP"])
  NBA[i, "EFF"] <- eff
}

rm(information, file,  filenames, i, name, year, eff)
