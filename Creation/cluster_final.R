
# Collect OND and MND NCAA years
summarise(group_by(NCAA, Player), S=min(Season))->f.year
f.year<-f.year[f.year$Player %in% NBA$Player,] # Should be NBA
freshman.year <-NCAA[0,]

colnames(freshman.year) <- colnames(NCAA)

cat("Collected names\n")

# Collect all NCAA freshman year data
for (p in 1:nrow(f.year)) {
  freshman.year[p,] <- NCAA[NCAA$Player==f.year$Player[p] & NCAA$Season==f.year$S[p],]
}

cat("Collected player data \n")

freshman.year <- freshman.year[!is.na(freshman.year$Player),]

colnames(freshman.year) <- colnames(NCAA)
# Aggregate it by player

# This will leave off percentages of most things, the breakdown of offensive and deffensive rebounds,
# and individual count of minutes played
#scaled.freshman <- data.frame(lapply(freshman.year[,-c(1,2,3,4,5,28)], scale))

#scaled.freshman <- data.frame(lapply(freshman.year[,-c(1,2,3,4,5,6,9,12,15,18,28)], scale))
#rownames(scaled.freshman) <- freshman.year$Player
#scaled.freshman$AST <- scaled.freshman$AST * 2
#scaled.freshman$TRB <- scaled.freshman$TRB * 2
#scaled.freshman$PTS <- scaled.freshman$PTS * 1.5
#scaled.freshman$MPperG <- scaled.freshman$MPperG * 3

#remove things
sf <- data.frame(lapply(freshman.year[,-c(1,2,3,4,5,6,9,12,15,18,28)], scale))
rownames(sf) <- freshman.year$Player
sf$AST <- sf$AST * 2
sf$TRB <- sf$TRB * 2
sf$PTS <- sf$PTS * 1.5
sf$MPperG <- sf$MPperG * 3
sf$FT <- sf$FT * 0.25
sf$FTA <- sf$FTA * 0.25

cat("Weighted frames \n")

#fresh.dist <- dist(scaled.freshman)
f.dist <- dist(sf)
#people <- data.frame(name=c("Ruth","Alex","Stephen"), age=c(22,22,46), height=c(5.5,6.4,6.2), IQ=c(200,200,180))
#scaled.people <- data.frame(lapply(people[2:4], scale))
#people.dist <- dist(scaled.people)

#fresh.dist.matrix <- as.matrix(fresh.dist)
#colnames(fresh.dist.matrix) <- freshman.year$Player
#rownames(fresh.dist.matrix) <- freshman.year$Player


fresh.matrix <- as.matrix(f.dist)
colnames(fresh.matrix) <- freshman.year$Player
rownames(fresh.matrix) <- freshman.year$Player

#people.dist.matrix <- as.matrix(people.dist)
#colnames(people.dist.matrix) <- people$name
#rownames(people.dist.matrix) <- people$name

#hclust(fresh.dist) -> fresh.results
hclust(f.dist) -> f.results
#hclust(people.dist) -> the.results

cat ("Defined clusters\n")

#Now you can look at the.results, and you should be able to dendogram() it, or
#w/e; look at https://cran.r-project.org/web/views/Cluster.html
#plot(as.dendrogram(fresh.results), hang= -1, horiz=T)

#clusters.fresh.5 = cutree(fresh.results, 5)
#clusters.fresh = cutree(fresh.results, 10)
#clusters.fresh.20 = cutree(fresh.results, 20)
#names(clusters.fresh.5)<- freshman.year$Player
#names(clusters.fresh)<-freshman.year$Player
#names(clusters.fresh.20)<- freshman.year$Player


num_clusters <- 8

#compare_dendlist <- dendlist(as.dendrogram(fresh.results), as.dendrogram(f.results))
#names(compare_dendlist) <- c("Original", "Final")

#compare <- cbind(cutree(fresh.results, num_clusters), cutree(f.results,num_clusters))
#compare <- as.data.frame(compare)

#compare_cor <- cor.dendlist(compare_dendlist)

# Collect Clusters
clusters <- cutree(f.results, num_clusters)

# Short List NBA data
group_by(NBA, Player)->NBA.years

NBA.years<-NBA.years[NBA.years$Player %in% f.year$Player,] # Should be in clusters

# Pick analysis type
type <- readline("Enter what type of analysis you want to see: (firstyear, average, topthree)")

for (c in 1:num_clusters){
  players <- names(clusters[clusters==c])
  if (type=="firstyear") {
    c.year <- summarise(group_by(NBA.years, Player), S=min(Years))
    c.year<-c.year[c.year$Player %in% players,]
    first.year <- data.frame(NBA[0,c(2,1,28)])
    colnames(first.year) <- c("Player", "Years", "EFF")
    for (p in 1:nrow(c.year)) {
      first.year[p,"EFF"] <- NBA[NBA$Player==c.year$Player[p] & NBA$Years==c.year$S[p],"EFF"]
      first.year[p, "Player"] <- c.year$Player[p]
      first.year[p,"Years"] <- c.year$S[p]
    } 
    plot(first.year$Years, first.year$EFF, main=c)
  } else if (type=="average") {
    yearsAtCollege <- summarise(group_by(NCAA, Player), num=n())
    c.year <- summarise(group_by(NBA.years, Player), EFF=mean(EFF))
    c.year<-c.year[c.year$Player %in% players,] 
    yearsAtCollege <- yearsAtCollege[yearsAtCollege$Player %in% players,]
    yearsAtCollege <- merge(c.year, yearsAtCollege)
    plot(yearsAtCollege$EFF, yearsAtCollege$num, main=c)
  }
}