type <- readline("Enter what type of analysis you want to see: (Average, Max, YearsToMax, AvgTopThree, YearsOverAvg) ")

for (i in seq(1:num_clusters)) {
  fresh <- eff_frame[eff_frame$Cluster==i & eff_frame$CollegeYears==1,]
  other <- eff_frame[eff_frame$Cluster==i & eff_frame$CollegeYears!=1,]
  
  if (type == "YearsToMax") {
    # Normalize since some people will have a max their first and only year
    
  }
  else {
    cat(i)
    fresh <- fresh[,type]
    other <- other[,c(type, "CollegeYears")]
    title = paste(i, "-", type, sep=" ")
    values <- eff_frame[,type]
    plot(rep(1, length(fresh)), fresh, main=title, xlim=c(1,5), col="red", ylim=c(min(values), max(values)))
    points(other[, "CollegeYears"], other[,type], col="blue")
    if (length(fresh) > 1) {
      t.results <- t.test(fresh, other[,type])
      cat(": P-Value - ", t.results$p.value, " and Confidence Interval - ", t.results$conf.int)
      cat("\n")
    }
    else {
      cat(": Not enough freshmen.\n")
    }
    
    
  }
}