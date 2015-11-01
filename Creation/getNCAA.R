require("XML")
require("plyr")

NCAA <- data.frame()
badnames <- c()
links <- c()
cat("Each Letter\n")
for (let in letters[1:26]){
  print(let)
  # Get all letter pages
  site = paste("http://www.sports-reference.com/cbb/players/",let,"-index.html", sep = "")
  con <- file(site)
  htmlData <- readLines(url(site))
  close(con)
  # Find all player links, then parse them
  lines <- grep(".*/cbb/players/.*[1-9].html", htmlData)
  
  # Parse player links to find repeated players
  for (i in 2:length(lines)) {
    step <-strsplit(htmlData[lines[i]], "[<>]")[[1]]
    step <- step[grep(".*cbb/players/.*",step)]
    player <- strsplit(step, '/')[[1]][4]
    player <- substring(player,1,nchar(player)-1)
    
    # If there are multiple names aka 2 Mike Smiths, put them in a bad list and skip all but the first, remove the first later
    if (strsplit(player,'[-.]')[[1]][3] != 1) {
      p <- paste(strsplit(player,'[-]')[[1]][1], strsplit(player, '[-]')[[1]][2], sep="-")
      badnames <- c(badnames, p)
    } else{
      # Save all the links
      p_site <- paste("http://www.sports-reference.com/cbb/players/", player, sep="")
      links <- c(links, p_site)
    }
  }
}

cat("Removing Bad Names\n")
badnames <- unique(badnames)
nums <- c()
for (i in 1:length(badnames)) {
  if (i%%100 == 0){
    print(i)
  }
  # Look for the requisit bad name in links
  num <- grep(paste(".*", badnames[i], ".*", sep=""),links)
  nums <- c(nums, num)
}

#Remove duplicate names so only unique names remain
links <- links[-nums]

cat("Getting player data\n")
for (i in 1:length(links)) {
  if (i%%250 == 0){
    print(i)
  }
  site <- links[i]
  con <- file(site)
  tables <- readHTMLTable(site)
  close(con)
  
  #Getting the name
  preName <- strsplit(links[i],"/")[[1]][6]
  p <- strsplit(preName,"[.-]")[[1]]
  name <- paste(p[2],p[1],sep=",")
  
  #Reading the table in
  table <- cbind("Player"=name,tables$players_totals)
  
  NCAA <- rbind.fill(NCAA, table)
}
cat("Editing\n")
NCAA[,-c(4,28,29,30,31)]-> NCAA

for (i in 4:ncol(NCAA)){
  as.double(NCAA[,i]) -> NCAA[,i]
}
for (i in 1:2){
  as.character(NCAA[,i]) -> NCAA[,i]
}

