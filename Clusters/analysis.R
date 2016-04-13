type <- readline("Enter what type of analysis you want to see: (Average, Max, YearsToMax, AvgTopThree) ")

## For clusters
for (i in seq(1:num_clusters)) {
  fresh <- eff_frame[eff_frame$Cluster==i & eff_frame$CollegeYears==1,]
  other <- eff_frame[eff_frame$Cluster==i & eff_frame$CollegeYears!=1,]
  cat(i)
  if (type == "YearsToMax" || type=="AvgTopThree") {
    fresh <- fresh[fresh$NumYears > 4, type]
    other <- other[other$NumYears > 4,c(type,"CollegeYears")]
    values <- c(fresh, other[,type])
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
    cat(": #OND - ", length(fresh), " #MND - ", nrow(other), "\n")
    cat("P-Value - ", t.results$p.value, " and Confidence Interval - ", t.results$conf.int, "\n")
    cat("Means were: OND: ", mean(fresh), " MND:", mean(other[,type]), "\n")
    if (t.results$p.value < 0.05) {
      # Is this the right direction?
      cat("There is a statistial difference.\n")
    }
  }
  else {
    cat(": Not enough freshmen.\n")
  }
  cat("\n")
}
 

# For all players
fresh <- eff_frame[eff_frame$CollegeYears==1,]
other <- eff_frame[eff_frame$CollegeYears!=1,]
if (type == "YearsToMax" || type=="AvgTopThree") {
  fresh <- fresh[fresh$NumYears > 4, type]
  other <- other[other$NumYears > 4,c(type,"CollegeYears")]
  values <- c(fresh, other[,type])
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
cat(": #OND - ", length(fresh), " #MND - ", nrow(other), "\n")
cat("P-Value - ", t.results$p.value, " and Confidence Interval - ", t.results$conf.int, "\n")
cat("Means were: OND: ", mean(fresh), " MND:", mean(other[,type]), "\n")
if (t.results$p.value < 0.05) {
  # Is this the right direction?
  cat("There is a statistial difference.\n")
}
