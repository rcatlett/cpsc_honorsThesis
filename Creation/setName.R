for (j in 1:nrow(NBA)){
  name <- NBA[j,2]
  s <- strsplit(name, ",")[[1]]
  for (i in 1:2){
    s[i] <- paste(toupper(substring(s[i], 1,1)), substring(s[i], 2),sep="", collapse=" ")
  }
  NBA[j,2] <- paste(s[1], s[2], sep=', ', collapse="")
}