# Collect OND NCAA data
NCAA[NCAA$Player%in%ond,]->ond.NCAA

# This will leave off percentages of most things, the breakdown of offensive and deffensive rebounds,
# and individual count of minutes played
scaled.ond <- data.frame(lapply(ond.NCAA[,c(5,7,8,10,11,13,14,18,21,22,23,24,25,26,27,28,29)], scale))
rownames(scaled.ond) <- ond.NCAA$Player

weighted.ond <- scaled.ond
# Now adjust for weights
weighted.ond$MPperG <- weighted.ond$MPperG * 4
weighted.ond$PTS <- weighted.ond$PTS * 2
weighted.ond$TRB <- weighted.ond$TRB * 2
weighted.ond$AST <- weighted.ond$AST * 2

weighted5.ond <-  weighted.ond
weighted25.ond <-  weighted.ond

weighted5.ond$"FT." <- weighted5.ond$"FT." * 0.5
weighted25.ond$"FT." <- weighted25.ond$"FT." * 0.25


ond.dist <- dist(scaled.ond)
weight5.dist <- dist(weighted5.ond)
weight25.dist <- dist(weighted25.ond)
#people <- data.frame(name=c("Ruth","Alex","Stephen"), age=c(22,22,46), height=c(5.5,6.4,6.2), IQ=c(200,200,180))
#scaled.people <- data.frame(lapply(people[2:4], scale))
#people.dist <- dist(scaled.people)

ond.dist.matrix <- as.matrix(ond.dist)
colnames(ond.dist.matrix) <- ond.NCAA$Player
rownames(ond.dist.matrix) <- ond.NCAA$Player

weight5.dist.matrix <- as.matrix(weight5.dist)
colnames(weight5.dist.matrix) <- ond.NCAA$Player
rownames(weight5.dist.matrix) <- ond.NCAA$Player

weight25.dist.matrix <- as.matrix(weight25.dist)
colnames(weight25.dist.matrix) <- ond.NCAA$Player
rownames(weight25.dist.matrix) <- ond.NCAA$Player
#people.dist.matrix <- as.matrix(people.dist)
#colnames(people.dist.matrix) <- people$name
#rownames(people.dist.matrix) <- people$name

hclust(ond.dist) -> the.results
hclust(weight5.dist) -> weighted5.results
hclust(weight25.dist) -> weighted25.results
#hclust(people.dist) -> the.results

#Now you can look at the.results, and you should be able to dendogram() it, or
#w/e; look at https://cran.r-project.org/web/views/Cluster.html
plot(as.dendrogram(the.results), hang= -1, horiz=T)
plot(as.dendrogram(weighted5.results), hang= -1, horiz=T)
plot(as.dendrogram(weighted25.results), hang= -1, horiz=T)

clusters.5 = cutree(the.results, 5)
clusters = cutree(the.results, 10)
clusters.20 = cutree(the.results, 20)
names(clusters.5)<- ond
names(clusters)<- ond
names(clusters.20)<- ond


weighted5.5 = cutree(weighted5.results, 5)
weighted5 = cutree(weighted5.results, 10)
weighted5.20 = cutree(weighted5.results, 20)
names(weighted5.5)<- ond
names(weighted5)<- ond
names(weighted5.20)<- ond

weighted25.5 = cutree(weighted25.results, 5)
weighted25 = cutree(weighted25.results, 10)
weighted25.20 = cutree(weighted25.results, 20)
names(weighted25.5)<- ond
names(weighted25)<- ond
names(weighted25.20)<- ond