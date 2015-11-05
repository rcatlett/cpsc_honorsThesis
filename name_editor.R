for (i in 1:nrow(NCAA)) {
  old_name <- strsplit(NCAA[i,2], " ")[[1]]
  new_name <- paste(old_name[2], old_name[1], sep = ", ")
  NCAA[i,2] <- new_name
}

for (i in 1:nrow(NBA)) {
  old_name <- strsplit(NBA[i,2], " ")[[1]]
  new_name <- paste(old_name[2], old_name[1], sep = ", ")
  NBA[i,2] <- new_name
}