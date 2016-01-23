library(dplyr)
# Get vector of Players who only have one year in the NCAA and those who have more than one year
summarise(group_by(NCAA, Player), years=n(),eff=mean(EFF)) -> players.by.year
filter(players.by.year, years==1)->one.and.done
filter(players.by.year, years<1)->more.and.done
one.and.done <- one.and.done[,c(1,3)]

cat("Separting OND and MND\n")
# Checking ones who are one-and-dones in the NBA
ond <- c()
for (i in one.and.done$Player) {
  if((is.element(i,NBA$Player))){
    ond <- c(ond, i)
  }
}

# Checking ones who are more-and-dones in the NBA
mnd <- c()
for (i in more.and.done$Player) {
  if((is.element(i,NBA$Player))){
    mnd <- c(mnd, i)
  }
}

cat("Aggregating EFFs\n")
# columns every year, NA where applicable, EFF where exists
aggregate.NBA <- data.frame(matrix(NA, nrow=0, ncol=27))
colnames(aggregate.NBA) <- c("Player", levels(NBA$Years))
players <- unique(NBA$Player)
for (p in players){
  r <- nrow(aggregate.NBA)+1
  aggregate.NBA[r,]$Player <- p
  effs <- NBA[NBA$Player==p,c("Years", "EFF")]
  for (y in effs$Years){
    aggregate.NBA[r,y] <- effs[effs$Years==y,"EFF"]
  }
}

cat("Tidying Data\n")
# Tidy this data
gather(aggregate.NBA, "year", "eff", 2:27) -> agg.NBA
# Removing NAs
agg.NBA[!(is.na(agg.NBA$eff)),]->agg.NBA
rownames(agg.NBA)<-c()

cat("Calculting Other EFF measures\n")
agg.groups <- group_by(agg.NBA,Player)
# find player's average and max effs
summarise(agg.groups, avgEff=mean(eff))->avg.effs
summarise(agg.groups, maxEff=max(eff))->max.effs
# fine effAvgTop3 and yearToMax
years.to.max <- data.frame(matrix(nrow=0,ncol=2))
colnames(years.to.max) <- c("Player", "to.max")
effAvgTop3 <- data.frame(matrix(nrow=0,ncol=2))
colnames(effAvgTop3) <- c("Player", "effAvgTop3")

players <- unique(agg.NBA$Player)
for (p in players){
  player.info <- agg.NBA[agg.NBA$Player==p,]
  ordered.rows.effs <- order(agg.NBA[agg.NBA$Player==p,"eff"], decreasing=TRUE)
  l <- length(ordered.rows.effs)
  if(l > 3) {
    l <- 3
  }
  avgTop3 <- mean(player.info[ordered.rows.effs[1:l],"eff"])
  toMax <- ordered.rows.effs[1] - 1
  r <- nrow(years.to.max)+1
  years.to.max[r,"Player"] <- p
  years.to.max[r,"to.max"] <- toMax
  effAvgTop3[r,"Player"] <- p
  effAvgTop3[r,"effAvgTop3"] <- avgTop3
}

cat("Merging Calculated EFFs\n")
# Merge all calculated effs
calculated.effs <- merge(avg.effs, max.effs)
calculated.effs <- merge(calculated.effs, years.to.max)
calculated.effs <- merge(calculated.effs, effAvgTop3)
# Add the calculated effs to the aggregate table

as.character(agg.NBA$year)-> agg.NBA$year
colnames(agg.NBA) <- c("Player", "quantity", "eff")
calculated.effs <- gather(calculated.effs, "quantity", "eff", 2:5)
as.character(calculated.effs$quantity) -> calculated.effs$quantity
rbind(agg.NBA, calculated.effs)-> agg.NBA

cat("Cleaning Environment\n")
rm(max.effs, years.to.max, effAvgTop3, avg.effs, calculated.effs, players, agg.groups, aggregate.NBA)
cat("Done with Data\n")

cat("Graphing calculated Effs")
# Viewing Hist of each
# Average
hist(agg.NBA[agg.NBA$quantity=="avgEff","eff"], main="Average EFF", xlim=c(-10,40), xlab="EFF", ylab="Frequency", ylim=c(0, 1000))
# Average Top 3
hist(agg.NBA[agg.NBA$quantity=="effAvgTop3","eff"], main="Average of Top 3 EFFs", xlim=c(-10,40), xlab="EFF", ylab="Frequency", ylim=c(0, 1000))
# Max
hist(agg.NBA[agg.NBA$quantity=="maxEff","eff"], main="Max Eff measure", xlim=c(-10,40), xlab="EFF", ylab="Frequency", ylim=c(0, 1000))
# Years to Max
hist(agg.NBA[agg.NBA$quantity=="to.max","eff"], main="Years to Max EFF", xlim=c(0,15), xlab="Years", ylab="Frequency", ylim=c(0, 1000))






