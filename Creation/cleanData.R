require("RCurl")
require("stringr")

#NBA Data

#for each year in the NBA database
readyYear <- function(year, data) {
  findYears <- paste("/", year, sep="")
  rows <- grep(findYears, data)
  fileName <- paste("y", year, ".csv", sep="")
  write.table(data[rows], file=fileName, na = "NA", row.names=FALSE, col.names = FALSE, quote = FALSE)
  information <- read.table(file =fileName, sep="", stringsAsFactor=FALSE)
  
  if (information[1,2] == "Player") {
    colNames <- information[1,]
    colnames(information) <- c("Years", colNames[2:length(colNames)])
    information <- information[-1,]
  }
  
  return (information)
}

#makes player row
clean <- function(data) {
  results <- c(0)
  for (i in 1:length(data)) {
    player <- strsplit(data[i], " ")[[1]]
    pos <- grep("[0-9A-Za-z]+", player)
    player <- player[pos]
    #if (length(player) == 23) {
  #    testNBA<-rbind(testNBA, c(player[1:19], NA, player[23], player[20:22], NA))
      results <- c(results, length(player))
      #} else if (length(player) == 25) {
     #   results <- c(results, 25)
  #    testNBA<-rbind(testNBA, player)
   # }
  }
  return (results)
}

#adds extra columns as needed
fixFormat <- function(year) {
  temp <- year[,1:19]
  temp <- cbind(temp, "DQ" = c(rep(NA, nrow(year))))
  temp <- cbind(temp, "PTS" = year[,20])
  temp <- cbind(temp, "TC" = c(rep(NA, nrow(year))), "EJ" = c(rep(NA, nrow(year))), "FF" = c(rep(NA, nrow(year))), "Sta" = c(rep(NA, nrow(year))), "+/-" = c(rep(NA, nrow(year))))
  return (temp)
}


fixCols <- function (data) {
  year <- ""
  for (i in 1:nrow(data)) {
    year <- strsplit(data[i,1], "")[[1]]
    if (year[1] == "/") {
      year <- year[-1]
    }
    year <- paste(year, sep="", collapse = "")
    data[i,1] <- year
  }

  return (data)
}

#Changes necessary columns to ints instead of strings
changeInts <- function (data) {
  for (i in 5:26) {
    as.integer(data[,i]) -> data[,i]
  }
  return(data)
}

capitalize <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

modifyName <- function(data) {
  name <- as.character(data[2])
  name <- strsplit(name, ",")[[1]]
  
  for (i in 1:length(name)) {
    s <- name[i]
    name[i] <- paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  name <- paste(name[2], name[1], collapse=" ")
  
  return(name)
}

#NCAA

#gets player position
grabPos <- function(webname, data.frame) {
  con <- file(webname)
  htmlData <- readLines(con)
  close(con)
  
  name <- htmlData[grep("<title>.*</title>", htmlData)]
  htmlData <- htmlData[grep("<b>Position:</b>.*<br>", htmlData)]
  htmlData <- gsub("^.*/b> ","", htmlData)
  htmlData <- gsub("<br>","", htmlData)
  
  for (i in 1:nrow(data.frame)) {
    if (data.frame[i,1] == name) {
      data.frame[i,2] <- htmlData
    }
  }

  return(data.frame)
}

#finds players by letter
collegeData <- function(webName) {
  con <- file(webName)
  htmlData <- readLines(url(webName))
  close(con)
  webName <- strsplit(webName, "/")
  webName <- paste(webName[[1]][1:4], collapse="/")
  lines <- c(0)
  for (i in 1:length(htmlData)) {
    lines <- c(lines, grep(".*playerpage.htm?.*", htmlData))
  }
  
  htmlData <- htmlData[c(lines)]
  htmlData <- unique(htmlData)
  return (htmlData)
}

#finds all the players links directly
getWebPages <- function(input, host) {
  for (i in 1:length(input)) {
    input[i] <- strsplit(input[i], " ")[[1]][2]
    input[i] <- strsplit(input[i], "\"")[[1]][2]
    input[i] <- paste(host, input[i], sep="/")
  }
  return (input)
}

#takes all players of a letter and sorts through to find college data
grabCollege <- function(site, dataFrame, rowNumNow) {
  con <- file(site)
  htmlData <- readLines(url(site))
  close(con)
  name <- grep("<title>.*</title>", htmlData)
  name <- htmlData[name]
  
  rowStart <- grep(".*College stats.*", htmlData)
  
  if (length(rowStart) != 0){
    rows <- grep(".*pre><p>.*", htmlData)
    rowEnd <- length(htmlData)
    for (i in 1:length(rows)) {
      if (rows[i] <= rowEnd && rows[i] > rowStart) {
        rowEnd <- rows[i]
      }
    }
    
    htmlData <- htmlData[(rowStart+2):(rowEnd-3)]
    
    colHeaders <- grep(".*<b>*", htmlData)
    throwAway <- grep(".*TOTALS.*", htmlData)
    throwAway <- c(throwAway, grep(".*---.*", htmlData))
    throwAway <- c(throwAway, colHeaders)
    colHeaders <- htmlData[colHeaders]
    
    temp <- c("")
    for (i in 1:length(colHeaders)) {
      line <- strsplit(colHeaders[i], " ")[[1]]
      for (j in 1:length(line)) {
        temp <- c(temp, line[j])
      }
    }
    
    colHeaders <- c("Years", "Player", "Team")
    temp <- temp[-c(grep("<.*>", temp))]
    temp <- temp[c(grep("[0-9A-Za-z].*", temp))]
    colHeaders <- c(colHeaders, temp)
    
    htmlData <- htmlData[-c(throwAway)]
    
    breakline <- grep("^$", htmlData)
    if(length(breakline)== 0) {
      breakline <- grep("^[[:space:]]*$", htmlData)
    }
    
    if (length(breakline) > 1){
      bline <- breakline[1]
      remove <- breakline[(-1)]
      htmlData <- htmlData[-remove]
    }
    
    line <- c("")
    for (i in 1:length(htmlData)) {
      line <- c(line, strsplit(htmlData[i], " "))
    }
    
    for (i in 2:breakline) {
      temp <- line[[i]]
      data <- temp[c(grep("[0-9A-Za-z].*", temp))]
      temp <- line[[i+breakline]]
      second <- grep("[0-9A-Za-z].*", temp)
      second <- second[-c(1,2)]
      data <- c(data, second)
      data <- c(data[1], name, data[2:27])
      dataFrame <- rbind(dataFrame, data)
      dataFrame[,] <- sapply(dataFrame[,], as.character)
    }
  } else {
    cat(rowNumNow, "\n")  
  }
  
  return(dataFrame)
}

#fixes format of year columns
clean.years <- function(data) {
  for (i in 1:nrow(data)) {
    year <- data[i, "Years"]
    year <- c(year, strsplit(year, ""))[[2]]
    no <- c(0)
    for (j in 1:length(year)) {
      if (year[j] == "<" ||year[j] == "/"|| year[j] == "b" ||year[j] == ">") {
        no <- c(no, j)
      }
    }
    
    if (length(no) > 1) {year <- year[-no]}
    
    year <- paste(year[1:5], collapse="")
    
    data[i, "Years"] <- year
  }
  return(data)
}

#fix names
fixName <- function(dataFrame) {
  for (i in 1:nrow(dataFrame)) {
    dataFrame[i,"Player"] <- gsub("^<title>","", dataFrame[i,"Player"])
    dataFrame[i,"Player"] <- gsub(" Past.*$","", dataFrame[i,"Player"])
  }
  return(dataFrame)
}