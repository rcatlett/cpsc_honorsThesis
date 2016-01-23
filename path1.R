library(dplyr)
# Get vector of Players who only have one year in the NCAA and those who have more than one year
summarise(group_by(NCAA, Player), years=n(),eff=mean(EFF)) -> players.by.year
filter(players.by.year, years==1)->one.and.done
filter(players.by.year, years<1)->more.and.done
one.and.done <- one.and.done[,c(1,3)]

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
