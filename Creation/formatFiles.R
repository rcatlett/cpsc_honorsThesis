/Fix all columns to have proper headers

filenames <- Sys.glob("y*.csv")
for (file in filenames){
  name <- file
  information <- read.csv(file, sep="", stringsAsFactor=FALSE)
  information <- information[-1,]
  if (ncol(information) == 26) {
    information <- information[,-26]
  }
  if (ncol(information) == 25) {
    colnames(information) <- c("Years", "Player", "Team", "PS", "GP", "MIN", "FGM", "FGA","3M", "3A", "FTM", "FTA", "OR", "TR", "AS", "ST", "TO", "BK", "PF", "DQ", "PTS", "TC", "EJ", "FF", "Sta")
  } else if (ncol(information) == 24) {
    colnames(information) <- c("Years", "Player", "Team", "PS", "GP", "MIN", "FGM", "FGA","3M", "3A", "FTM", "FTA", "OR", "TR", "AS", "ST", "TO", "BK", "PF", "PTS", "TC", "EJ", "FF", "Sta")
  } else if(ncol(information) == 20) {
    information <- read.csv(file, sep="", header=FALSE, stringsAsFactor=FALSE)
    colnames(information) <- c("Years", "Player", "Team", "PS", "GP", "MIN", "FGM", "FGA", "3M", "3A", "FTM", "FTA", "OR", "TR", "AS", "ST", "TO", "BK", "PF", "PTS")
  }
  write.table(information, file=name, row.names=FALSE, quote=FALSE)
}