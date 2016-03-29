library(dplyr)
library(tidyr)
# Get vector of Players who only have one year in the NCAA and those who have more than one year
summarise(group_by(NCAA, Player), years=n(),eff=mean(EFF)) -> players.by.year
filter(players.by.year, years==1)->one.and.done
filter(players.by.year, years>1)->more.and.done
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
# find effAvgTop3 and yearToMax
years.to.max <- data.frame(matrix(nrow=0,ncol=2))
colnames(years.to.max) <- c("Player", "to.max")
effAvgTop3 <- data.frame(matrix(nrow=0,ncol=2))
colnames(effAvgTop3) <- c("Player", "effAvgTop3")
# find minimum and difference over career
minimum.eff <- data.frame(matrix(nrow=0,ncol=2))
colnames(minimum.eff) <- c("Player", "min")
#difference.in.eff <- data.frame(matrix(nrow=0,ncol=2))
#colnames(difference.in.eff) <- c("Player", "diff")

players <- unique(agg.NBA$Player)
for (p in players){
  player.info <- agg.NBA[agg.NBA$Player==p,]
  ordered.rows.effs <- order(agg.NBA[agg.NBA$Player==p,"eff"], decreasing=TRUE)
  l <- length(ordered.rows.effs)
  min.eff <- agg.NBA[agg.NBA$Player==p,"eff"][l]
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
  minimum.eff[r,"Player"] <- p
  minimum.eff[r,"min"] <- min.eff
#  difference.in.eff[r,"Player"] <- p
#  difference.in.eff[r,"diff"] <- agg.NBA[agg.NBA$Player==p,"eff"][1] - min.eff
}

cat("Merging Calculated EFFs\n")
# Merge all calculated effs
calculated.effs <- merge(avg.effs, max.effs)
calculated.effs <- merge(calculated.effs, years.to.max)
calculated.effs <- merge(calculated.effs, effAvgTop3)
calculated.effs <- merge(calculated.effs, minimum.eff)
#calculated.effs <- merge(calculated.effs, difference.in.eff)
# Add the calculated effs to the aggregate table

as.character(agg.NBA$year)-> agg.NBA$year
colnames(agg.NBA) <- c("Player", "quantity", "eff")
calculated.effs <- gather(calculated.effs, "quantity", "eff", 2:6)#7)
as.character(calculated.effs$quantity) -> calculated.effs$quantity
rbind(agg.NBA, calculated.effs)-> agg.NBA

#cat("Cleaning Environment\n")
#rm(max.effs, years.to.max, effAvgTop3, avg.effs, calculated.effs, players, agg.groups, aggregate.NBA)
#rm(y, toMax, r, p, ordered.rows.effs, l, i, eff, avgTop3)
# THIS! rm(agg.groups,avg.effs,avgEff, calculated.effs, combined, difference.in.eff, early.peak, effAvgTop3, effs, max.effs, minimum.eff,people, player.info, years.to.max, avgTop3, i, iverson, l, min.eff, p, players, r, toMax, y)
cat("Graphing calculated Effs\n")
# Viewing Hist of each
# Average
par(mfrow=c(1,2))
hist(agg.NBA[agg.NBA$quantity=="avgEff","eff"], main="Average EFF", xlim=c(-10,40), xlab="EFF", ylab="Frequency", ylim=c(0, 1000))
qqnorm(agg.NBA[agg.NBA$quantity=="avgEff","eff"], main="Average EFF")
qqline(agg.NBA[agg.NBA$quantity=="avgEff","eff"])
# Average Top 3
hist(agg.NBA[agg.NBA$quantity=="effAvgTop3","eff"], main="Average of Top 3 EFFs", xlim=c(-10,40), xlab="EFF", ylab="Frequency", ylim=c(0, 1000))
qqnorm(agg.NBA[agg.NBA$quantity=="effAvgTop3","eff"], main="Average Top 3 EFFs")
qqline(agg.NBA[agg.NBA$quantity=="effAvgTop3","eff"])
# Max
hist(agg.NBA[agg.NBA$quantity=="maxEff","eff"], main="Max EFF measure", xlim=c(-10,40), xlab="EFF", ylab="Frequency", ylim=c(0, 1000))
qqnorm(agg.NBA[agg.NBA$quantity=="maxEff","eff"], main="Max EFF")
qqline(agg.NBA[agg.NBA$quantity=="maxEff","eff"])
# Years to Max
par(mfrow=c(1,1))
hist(agg.NBA[agg.NBA$quantity=="to.max","eff"], main="Years to Max EFF", xlim=c(0,15), xlab="Years", ylab="Frequency", ylim=c(0, 1500))
# Difference in Max and Min
#hist(agg.NBA[agg.NBA$quantity=="diff","eff"], main="Difference in EFF", xlim=c(-25,35), xlab="EFF", ylab="Frequency", ylim=c(0, 1000))

cat("Separating OND and MND\n")
# Separate Out OND and MND
agg.NBA[agg.NBA$Player%in%ond,]->ond.agg
agg.NBA[agg.NBA$Player%in%mnd,]->mnd.agg

cat("T-Tests\n")
# Average
t.test(ond.agg[ond.agg$quantity=="avgEff","eff"], mnd.agg[mnd.agg$quantity=="avgEff","eff"])
# Average Top 3
t.test(ond.agg[ond.agg$quantity=="effAvgTop3","eff"], mnd.agg[mnd.agg$quantity=="effAvgTop3","eff"])
# Max
t.test(ond.agg[ond.agg$quantity=="maxEff","eff"], mnd.agg[mnd.agg$quantity=="maxEff","eff"])
# Years to Max
t.test(ond.agg[ond.agg$quantity=="to.max","eff"], mnd.agg[mnd.agg$quantity=="to.max","eff"])

# People who peaked in 0 or 1 years
agg.NBA[agg.NBA$quantity=="to.max" & agg.NBA$eff %in% c(0,1),] -> early.peak
early.peak$Player -> early.peak.players
cat("All Done\n")
