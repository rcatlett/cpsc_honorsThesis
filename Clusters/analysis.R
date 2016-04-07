type <- readline("Enter what type of analysis you want to see: (Average, Max, YearsToMax, AvgTopThree, YearsOverAvg) ")

## For clusters
for (i in seq(1:num_clusters)) {
  fresh <- eff_frame[eff_frame$Cluster==i & eff_frame$CollegeYears==1,]
  other <- eff_frame[eff_frame$Cluster==i & eff_frame$CollegeYears!=1,]
  cat(i)
  if (type == "YearsToMax") {
    fresh <- fresh[, type] / fresh[,"NumYears"]
    other <- other[,c(type,"CollegeYears", "NumYears")]
    other$"YearsToMax" <- other$"YearsToMax"/other$"NumYears"
    other <- other[,-3]
    values <- eff_frame[,"YearsToMax"]/eff_frame[,"NumYears"]
  }
  else {
    fresh <- fresh[,type]
    other <- other[,c(type, "CollegeYears")]
    values <- eff_frame[,type]
  }
  
  title = paste(i, "-", type, sep=" ") 
  plot(rep(1, length(fresh)), fresh, main=title, xlim=c(1,5), col="red", ylim=c(min(values), max(values)))
  points(other[, "CollegeYears"], other[,type], col="blue")
  if (length(fresh) > 1) {
    t.results <- t.test(fresh, other[,type])
    cat(": P-Value - ", t.results$p.value, " and Confidence Interval - ", t.results$conf.int)
    cat("\n")
    if (t.results$p.value < 0.05) {
      # Is this the right direction?
      cat("There is a statistial difference.\n")
    }
  }
  else {
    cat(": Not enough freshmen.\n")
  }
}
 

# For all players
fresh <- eff_frame[eff_frame$CollegeYears==1,]
other <- eff_frame[eff_frame$CollegeYears!=1,]
if (type == "YearsToMax") {
  fresh <- fresh[, type] / fresh[,"NumYears"]
  other <- other[,c(type,"CollegeYears", "NumYears")]
  other$"YearsToMax" <- other$"YearsToMax"/other$"NumYears"
  other <- other[,-3]
  values <- eff_frame[,"YearsToMax"]/eff_frame[,"NumYears"]
} else {
  fresh <- fresh[,type]
  other <- other[,c(type, "CollegeYears")]
  values <- eff_frame[,type]
}
cat("All")
title = paste("All -", type, sep=" ") 
plot(rep(1, length(fresh)), fresh, main=title, xlim=c(1,5), col="red", ylim=c(min(values), max(values)))
points(other[, "CollegeYears"], other[,type], col="blue")
t.results <- t.test(fresh, other[,type])
cat(": P-Value - ", t.results$p.value, " and Confidence Interval - ", t.results$conf.int)
cat("\n")
if (t.results$p.value < 0.05) {
  # Is this the right direction?
  cat("There is a statistial difference.\n")
}
