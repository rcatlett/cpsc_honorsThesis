# What I ran

NCAA %>% group_by(Player) %>% summarise(nYears = n()) -> nYears
nYears <- tbl_df(nYears)
nYears %>% arrange(desc(nYears)) -> nYears
#There are some with 5 and 6 years (change schools or grad school - acceptable range)

merge(NBA, nYears, by="Player") -> combined
combined <- tbl_df(combined)

avgEff <- combined %>% group_by(Player) %>% summarise(avgEFF = mean(EFF), years = mean(nYears)) 

# Removes NCAA players from NBA
myvars <- NBA$Player %in% NCAA$Player
noCollege <- NBA[!myvars,]
noCollegePlayers <- unique(noCollege$Player)

#Groups the nocollege with their efficiency
noCoEff <- noCollege %>% group_by(Player) %>% summarise(avgEFF = mean(EFF))

#Sets the graphical environment
par(mar=c(1,1,1,1))
par(mfrow=c(3,2))
#Prints the 6 graphs
boxplot(noCoEff$avgEFF, main="No College", ylim = c(-5,35))
boxplot(avgEff[avgEff$year == 1,2], main = "1 Year College", ylim = c(-5,35))
boxplot(avgEff[avgEff$year == 2,2], main = "2 Years College", ylim = c(-5,35))
boxplot(avgEff[avgEff$year == 3,2], main = "3 Years College", ylim = c(-5,35))
boxplot(avgEff[avgEff$year == 4,2], main = "4 Years College", ylim = c(-5,35))
boxplot(avgEff[avgEff$year == 5,2], main = "5 Years College", ylim = c(-5,35))