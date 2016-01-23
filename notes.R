# mean points of each player
NBA %>% group_by(Player) %>% summarize(pts = mean(PTS))

# number of years in the table
NBA %>% summarize(numsYrs=n())

filter(NCAA, Player == "Funderburke, Lawrence")