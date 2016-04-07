# Run after clusters_final.R

eff_frame <- data.frame(matrix(ncol=9))
colnames(eff_frame) <- c("Player", "Cluster","CollegeYears", "Average", "Max", "YearsToMax", "NumYears", "AvgTopThree", "YearsOverAvg")
l <- 1                      
for (player in names(clusters)) {
  eff_frame[l,"Player"] <- player
  effs <- NBA[NBA$Player==player, "EFF"]
  eff_frame[l,"Cluster"] <- clusters[player]
  eff_frame[l, "CollegeYears"] <- nrow(NCAA[NCAA$Player==player,])
  eff_frame[l, "Average"] <- colMeans(effs)
  eff_frame[l, "Max"] <-max(effs)
  eff_frame[l, "YearsToMax"] <- which.max(effs$EFF)
  eff_frame[l, "NumYears"] <- nrow(effs)
  eff_frame[l, "AvgTopThree"] <- mean(sort(effs$EFF)[1:min(3, length(effs$EFF))])
  eff_frame[l, "YearsOverAvg"] <- nrow(effs[effs$EFF >=colMeans(effs),])
  if (l %% 100 == 0) {
    
    cat(l, "\n")
  }
  l <- l + 1
  
}

