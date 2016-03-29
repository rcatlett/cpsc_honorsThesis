require("RCurl")

grabData <- function(webName) {
  con <- file(webName)
  htmlData <- readLines(url(webName))
  close(con)
  lines <- (0)
  lines <- c(lines, grep(".*-.* Stats<.*", htmlData))
  links <- (0)
  for (i in 2:length(lines)) {
    temp <- strsplit(htmlData[lines[i]], "[.]")
    links <- c(links, temp[[1]][2])
  }
  finalData <- c(" ")
  for (i in 2:length(links)) {
    newLink <- paste(webName, links[i], "RD.html", sep = "")
    if (url.exists(newLink, .header=FALSE)) {
      newData <- readLines(url(newLink))
    } else {
      newLink <- paste(webName, links[i], "RD.txt", sep = "")
      newData <- readLines(url(newLink))
    }
    for (x in 2:length(newData)-1) {
      nextLine <- paste(links[i], newData[x], sep="  ")
      finalData <- c(finalData, nextLine)
    }
  }

  return (finalData)
}