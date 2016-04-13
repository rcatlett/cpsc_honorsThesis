
# For testing, smaller group
my_list <- c("Alford, Steve"   ,  "Anthony, Carmelo",  "Anthony, Greg" ,   "Battle, John" , "Beasley, Michael", "Bennett, Anthony" , "Blackwell, James"  ,
             "Brown, Lorenzo","Calloway, Rick"  , "Carr, Antoine"    ,  "Causwell, Duane"  ,"Chambers, Tom"    ,  "Childs, Chris"    ,"Cousins, Demarcus" ,
             "Crawford, Jamal"  ,"Curry, Michael" ,   "Curry, Stephen"  ,  "Davis, Antonio"  , "Drummond, Andre"  ,   "Durant, Kevin"  ,   "Eaton, Mark" ,
             "Ewing, Patrick"   ,  "Fuller, Todd"   , "Garner, Chris"   , "Goodwin, Archie"  , "Green, Jeff"  ,  "Higgins, Mike" , "Hughes, Larry", 
             "Iverson, Allen", "Jackson, Randall",  "Jackson, Randell" , "James, Henry", "Knight, Travis",  "Levingston, Cliff", "Liggins, Deandre", 
             "Livingston, Randy", "Love, Kevin",      "Miller, Darius", "Miller, Reggie", "Morris, Randolph", "Mottola, Hanno", "Murphy, Kevin", 
             "Murphy, Tod", "Nash, Steve", "O'neal, Shaquille", "Odom, Lamar", "Perkins, Sam", "Randolph, Anthony", "Rogers, Roy", "Sutton, Greg", 
             "Sykes, Larry", "Taylor, Jeffery", "Teague, Marquis", "Thomas, Irving", "Thornton, Bob", "Turner, John", "Wade, Dwyane", "Wall, John", 
             "Webb, Marcus", "Wright, Antoine")

job_set <- c("Anthony, Carmelo","Chambers, Tom" , "Cousins, Demarcus" ,  "Curry, Stephen"  ,  "Durant, Kevin"  , "Ewing, Patrick", "Goodwin, Archie"  , 
             "Iverson, Allen",  "Liggins, Deandre",  "Livingston, Randy", "Love, Kevin",  "O'neal, Shaquille", "Irving, Kyrie","Wade, Dwyane", "Wall, John")

my_list <- job_set

# Collect OND and MND NCAA years
summarise(group_by(NCAA, Player), S=min(Season))->f.year
f.year<-f.year[f.year$Player %in% my_list,] # Should be NBA
freshman.year <- data.frame(matrix(NA,ncol=ncol(NCAA), nrow=nrow(f.year)))

# Collect all NCAA freshman year data
for (p in 1:nrow(f.year)) {
  freshman.year[p,] <- NCAA[NCAA$Player==f.year$Player[p] & NCAA$Season==f.year$S[p],]
}
#freshman.year <- freshman.year[!is.na(freshman.year$Player),]

colnames(freshman.year) <- colnames(NCAA)
# Aggregate it by player

# This will leave off percentages of most things, the breakdown of offensive and deffensive rebounds,
# and individual count of minutes played
scaled.freshman <- data.frame(lapply(freshman.year[,-c(1,2,3,4,5,28)], scale))

#scaled.freshman <- data.frame(lapply(freshman.year[,-c(1,2,3,4,5,6,9,12,15,18,28)], scale))
rownames(scaled.freshman) <- freshman.year$Player
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


fresh.dist <- dist(scaled.freshman)
f.dist <- dist(sf)
#people <- data.frame(name=c("Ruth","Alex","Stephen"), age=c(22,22,46), height=c(5.5,6.4,6.2), IQ=c(200,200,180))
#scaled.people <- data.frame(lapply(people[2:4], scale))
#people.dist <- dist(scaled.people)

fresh.dist.matrix <- as.matrix(fresh.dist)
colnames(fresh.dist.matrix) <- freshman.year$Player
rownames(fresh.dist.matrix) <- freshman.year$Player


fresh.matrix <- as.matrix(f.dist)
colnames(fresh.matrix) <- freshman.year$Player
rownames(fresh.matrix) <- freshman.year$Player

#people.dist.matrix <- as.matrix(people.dist)
#colnames(people.dist.matrix) <- people$name
#rownames(people.dist.matrix) <- people$name

hclust(fresh.dist) -> fresh.results
hclust(f.dist) -> f.results
#hclust(people.dist) -> the.results

#Now you can look at the.results, and you should be able to dendogram() it, or
#w/e; look at https://cran.r-project.org/web/views/Cluster.html
#plot(as.dendrogram(fresh.results), hang= -1, horiz=T)

clusters.fresh.5 = cutree(fresh.results, 4)
clusters.fresh = cutree(fresh.results, 4)
clusters.fresh.20 = cutree(fresh.results, 4)
names(clusters.fresh.5)<- freshman.year$Player
names(clusters.fresh)<-freshman.year$Player
names(clusters.fresh.20)<- freshman.year$Player


compare_dendlist <- dendlist(as.dendrogram(fresh.results), as.dendrogram(f.results))
names(compare_dendlist) <- c("Not Weighted", "Weighted")

compare <- cbind(clusters.fresh, cutree(f.results,4))
compare <- as.data.frame(compare)

compare_cor <- cor.dendlist(compare_dendlist)
compare_dendlist %>% dendlist(which = c(1,2)) %>% 
  untangle(method = "step2side") %>% set("branches_k_color", k=4) %>%
  tanglegram()