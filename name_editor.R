#for (i in 1:nrow(NCAA)) {
 # old_name <- strsplit(NCAA[i,2], ", ")[[1]]
  #first_name <-paste(strsplit(old_name[2], "")[[1]][-c(1,2,3,length(strsplit(old_name[2], "")[[1]]))],sep='',collapse='')
#  last_name <-paste(strsplit(old_name[3], "")[[1]][-c(1,length(strsplit(old_name[3], "")[[1]])-1, length(strsplit(old_name[3], "")[[1]]))],sep='',collapse='')
#  new_name <- paste(last_name, first_name, sep=", ")
#  NCAA[i,2] <- new_name
#}

for (i in 1:nrow(NBAFULL)) {
  old_name <- strsplit(NBAFULL[i,2], " ")[[1]]
  new_name <- paste(old_name[2], old_name[1], sep = ", ")
  NBAFULL[i,2] <- new_name
}