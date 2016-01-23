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
cat("Done\n")
# edit this below
# add by filtering using ond and mnd
#aggregate.NBA[aggregate.NBA$Player%in%ond,]->ond.NBA
#aggregate.NBA[aggregate.NBA$Player%in%mnd,]->mnd.NBA
# check for normality (bell curve, historgram, qqplot)
#hist(one.and.done$eff, main="EFF of ONE-AND-DONES", xlab="EFF", ylab="Frequency", xlim=c(-5,35), breaks=16, prob=TRUE)
#lines(density(one.and.done$eff, na.rm=TRUE))
#hist(more.and.done$eff, main="EFF of MORE-AND-DONES", xlab="EFF", ylab="Frequency", xlim=c(-5,35), breaks=16, prob=TRUE)
#lines(density(more.and.done$eff, na.rm=TRUE))
#summarise(group_by(NBA, Years), avg.eff=mean(EFF))->NBA.season.EFF

# run t.test
