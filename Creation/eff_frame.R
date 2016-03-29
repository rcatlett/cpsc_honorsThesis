# Run after clusters_final.R

eff_frame <- data.frame(matrix(ncol=7))
colnames(eff_frame) <- c("Player", "Cluster", "Average", "Max", "YearsToMax", "NumYears", "AvgTopThree")
l <- 0                       
for (player in names(clusters)) {
  eff_frame[l+1,"Player"] <- player
  effs <- NBA[NBA$Player==player, "EFF"]
  eff_frame[l+1,"Cluster"] <- clusters[player]
  
  eff_frame[l+1, "Average"] <- colMeans(effs)
  eff_frame[l+1, "Max"] <-max(effs)
  eff_frame[l+1, "YearsToMax"] <- which.max(effs$EFF)
  eff_frame[l+1, "NumYears"] <- nrow(effs)
  eff_frame[l+1, "AvgTopThree"] <- mean(sort(effs$EFF)[1:min(3, length(effs$EFF))])
  
  l <- l + 1
  
}

