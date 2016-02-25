#install.packages("dendextend")
#install.packages("corrplot")
#library(dendextend)
#library(corrplot)
#library(colorspace)

compare_dendlist <- dendlist(as.dendrogram(the.results), as.dendrogram(weighted5.results), as.dendrogram(weighted25.results))
names(compare_dendlist) <- c("Results", "Weighted5","Wighted25")

compare <- cbind(clusters, weighted5, weighted25)
compare <- as.data.frame(compare)

compare_cor <- cor.dendlist(compare_dendlist)

par(mfrow = c(1,3))
all.values <- c(0, max(table(clusters.5)), max(table(weighted.5)))
colvec <- c(rgb(1,0,0,1/4), rgb(0,0,1,1/4))
#compare_dendlist %>% ladderize %>% 
#  set("branches_k_color", k=5) %>%
#  tanglegram(common_subtree_color_branches = TRUE)
barplot(table(clusters.5), col=rgb(1,0,0,1/4), ylim=c(min(all.values), max(all.values)), main="Compared Cluters 5")
barplot(table(weighted.5), col=rgb(0,0,1,1/4),add=T)
legend("topright",legend=c("Results","Weighted"), fill=colvec)

#compare_dendlist %>% dendlist(which = c(1,2)) %>% ladderize %>% 
#  set("branches_k_color", k=10) %>%
#  tanglegram(common_subtree_color_branches = TRUE)
barplot(table(clusters), col=rgb(1,0,0,1/4), ylim=c(min(all.values), max(all.values)), main="Compared Cluters 10")
barplot(table(weighted), col=rgb(0,0,1,1/4), add=T)
legend("topright",legend=c("Results","Weighted"), fill=colvec)

#compare_dendlist %>% ladderize %>% 
#  set("branches_k_color", k=20) %>%
#  tanglegram(common_subtree_color_branches = TRUE)
barplot(table(clusters.20),col=rgb(1,0,0,1/4), ylim=c(min(all.values), max(all.values)), main="Compared Cluters 20")
barplot(table(weighted.20),col=rgb(0,0,1,1/4), add=T)
legend("topright",legend=c("Results","Weighted"), fill=colvec)



