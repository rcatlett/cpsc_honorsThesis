install.packages("dendextend")
install.packages("corrplot")
library(dendextend)
library(corrplot)
library(colorspace)

# Collect OND NCAA data
NCAA[NCAA$Player%in%ond,]->ond.NCAA
ond.NCAA <- ond.NCAA[,c(1,5,7,8,10,11,13,14,18,21,22,23,24,25,27,28,29)]

# Collect MND NCAA data
NCAA[NCAA$Player%in%mnd,]->mnd.NCAA

# Aggregate it by player
mnd.NCAA <- group_by(mnd.NCAA, Player)
mnd.NCAA <- summarise(group_by(mnd.NCAA, Player), G=mean(G), FG=mean(FG), FGA=mean(FGA), 
          "2P"=mean("2P"), "2PA"=mean("2PA"), "3P"=mean("3P"), "3PA"=mean("3PA"), 
          "FT%"=mean("FT%"), TRB=mean(TRB), AST=mean(AST), STL=mean(STL),
          BLK=mean(BLK), TOV=mean(TOV), PTS=mean(PTS), EFF=mean(EFF), 
          MPperG=mean(MPperG))

# Compile together
all.done.NCAA <- rbind(ond.NCAA, mnd.NCAA)

# This will leave off percentages of most things, the breakdown of offensive and deffensive rebounds,
# and individual count of minutes played
scaled.all <- data.frame(lapply(all.done.NCAA[,-1], scale))
rownames(scaled.all) <- all.done.NCAA$Player

weighted.all <- scaled.all
# Now adjust for weights
weighted.all$MPperG <- weighted.all$MPperG * 4
weighted.all$PTS <- weighted.all$PTS * 2
weighted.all$TRB <- weighted.all$TRB * 2
weighted.all$AST <- weighted.all$AST * 2

weighted5.all <-  weighted.all
weighted25.all <-  weighted.all

weighted5.all$"FT." <- weighted5.all$"FT." * 0.5
weighted25.all$"FT." <- weighted25.all$"FT." * 0.25


all.dist <- dist(scaled.all)
weight5.all.dist <- dist(weighted5.all)
weight25.all.dist <- dist(weighted25.all)
#people <- data.frame(name=c("Ruth","Alex","Stephen"), age=c(22,22,46), height=c(5.5,6.4,6.2), IQ=c(200,200,180))
#scaled.people <- data.frame(lapply(people[2:4], scale))
#people.dist <- dist(scaled.people)

all.dist.matrix <- as.matrix(all.dist)
colnames(all.dist.matrix) <- all.done.NCAA$Player
rownames(all.dist.matrix) <- all.done.NCAA$Player

weight5.all.dist.matrix <- as.matrix(weight5.all.dist)
colnames(weight5.all.dist.matrix) <- all.done.NCAA$Player
rownames(weight5.all.dist.matrix) <- all.done.NCAA$Player

weight25.all.dist.matrix <- as.matrix(weight25.all.dist)
colnames(weight25.all.dist.matrix) <- all.done.NCAA$Player
rownames(weight25.all.dist.matrix) <- all.done.NCAA$Player
#people.dist.matrix <- as.matrix(people.dist)
#colnames(people.dist.matrix) <- people$name
#rownames(people.dist.matrix) <- people$name

hclust(all.dist) -> all.results
hclust(weight5.all.dist) -> weighted5.all.results
hclust(weight25.all.dist) -> weighted25.all.results
#hclust(people.dist) -> the.results

#Now you can look at the.results, and you should be able to dendogram() it, or
#w/e; look at https://cran.r-project.org/web/views/Cluster.html
plot(as.dendrogram(all.results), hang= -1, horiz=T)
plot(as.dendrogram(weighted5.all.results), hang= -1, horiz=T)
plot(as.dendrogram(weighted25.all.results), hang= -1, horiz=T)

clusters.all.5 = cutree(all.results, 5)
clusters.all = cutree(all.results, 10)
clusters.all.20 = cutree(all.results, 20)
names(clusters.all.5)<- all.done.NCAA$Player
names(clusters.all)<-all.done.NCAA$Player
names(clusters.all.20)<- all.done.NCAA$Player


weighted5.all.5 = cutree(weighted5.all.results, 5)
weighted5.all = cutree(weighted5.all.results, 10)
weighted5.all.20 = cutree(weighted5.all.results, 20)
names(weighted5.all.5)<-all.done.NCAA$Player
names(weighted5.all)<-all.done.NCAA$Player
names(weighted5.all.20)<- all.done.NCAA$Player

weighted25.all.5 = cutree(weighted25.all.results, 5)
weighted25.all = cutree(weighted25.all.results, 10)
weighted25.all.20 = cutree(weighted25.all.results, 20)
names(weighted25.all.5)<- all.done.NCAA$Player
names(weighted25.all)<- all.done.NCAA$Player
names(weighted25.all.20)<-all.done.NCAA$Player


# WHY THIS NO WORK!?
# dend_diff(dendlist(as.dendrogram(weighted25.all.results), 
#        as.dendrogram(weighted5.all.results)))
