# What I ran

NCAA %>% group_by(Player) %>% summarise(nYears = n()) -> nYears
nYears <- tbl_df(nYears)
nYears %>% arrange(desc(nYears)) -> nYears
#There are some with 5 and 6 years (change schools or grad school - acceptable range)

merge(NBA, nYears, by="Player") -> combined
combined <- tbl_df(combined)

avgEff <- combined %>% group_by(Player) %>% summarise(avgEFF = mean(EFF), years = mean(nYears)) 

myvars <- NBA$Player %in% NCAA$Player
noCollege <- NBA[!myvars,]
noCollegePlayers <- unique(noCollege$Player)

noCoEff <- noCollege %>% group_by(Player) %>% summarise(avgEFF = mean(EFF))
plot(avgEff$avgEFF, avgEff$years, main="College Years over EFF in NBA")
boxplot(noCoEff$avgEFF, main="Average EFF in NBA, No College")