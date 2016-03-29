# EFF = (PTS + REB + AST + STL + BLK − Missed FG − Missed FT − TO) / GP

#efficiencyNBA
#for (i in 1:nrow(NBA)) {
#  eff <- as.double((NBA[i,"PTS"] + NBA[i,"TR"] + NBA[i,"AS"] + NBA[i,"ST"] + NBA[i,"BK"] - (NBA[i, "FGA"]-NBA[i, "FGM"]) - (NBA[i,"FTA"]- NBA[i,"FTM"]) - NBA[i,"TO"])/NBA[i,"GP"])
#  NBA[i, "EFF"] <- eff
#}

# eficiency NCAA
#for (i in 1:nrow(NCAA)) {
 # eff <- as.double((NCAA[i,"PTS"] + NCAA[i,"TRB"] + NCAA[i,"AST"] + NCAA[i,"STL"] + NCAA[i,"BLK"] - (NCAA[i, "FGA"]-NCAA[i, "FG"]) - (NCAA[i,"FTA"]- NCAA[i,"FT"]) - NCAA[i,"TOV"])/NCAA[i,"G"])
#  NCAA[i, "EFF"] <- eff
#}

# Hollinger Game Score - for a single game
#pts + 0.4*fg - 0.7*fga - 0.4(fta-ft) + .7*orb + .3**drb + stl + 0.7*ast + 0.7*blk - 0.4*pf - tov
# 40 - outstanding, 10 is average
#for (i in 1:nrow(NCAA)) {
#  hgs <- as.double((NCAA[i,"PTS"] + 0.4*NCAA[i,"FG"] - 0.7*NCAA[i,"FGA"] - 0.4*(NCAA[i,"FTA"]-NCAA[i,"FT"]) + 0.7*NCAA[i,"ORB"] + 0.3*NCAA[i, "DRB"] + NCAA[i,"STL"] + 0.7*NCAA[i, "AST"] + 0.7*NCAA[i,"BLK"] - 0.4*NCAA[i,"PF"] - NCAA[i,"TOV"])/NCAA[i,"G"])
#  NCAA[i, "HGS"] <- hgs
#}
#for (i in 1:nrow(NBA)) {
#  hgs <- as.double((NBA[i,"PTS"] + 0.4*NBA[i,"FGM"] - 0.7*NBA[i,"FGA"] - 0.4*(NBA[i,"FTA"]-NBA[i,"FTM"]) + 0.7*NBA[i,"OR"] + 0.3*(NBA[i, "TR"]-NBA[i,"OR"]) + NBA[i,"ST"] + 0.7*NBA[i, "AS"] + 0.7*NBA[i,"BK"] - 0.4*NBA[i,"PF"] - NBA[i,"TO"])/NBA[i,"GP"])
#  NBA[i, "HGS"] <- hgs
#}

# Win Score
#pts + (orb + drb) + stl + .5*blk + .5*ast - fga - .5*fta - to - .5*pf
# Per minute - C .22, PF .21, SF .15, SG, PG .13
for (i in 1:nrow(NCAA)) {
  win.score <- as.double((NCAA[i,"PTS"] + NCAA[i,"ORB"]  + NCAA[i, "DRB"] + NCAA[i,"STL"] + 0.5*NCAA[i,"BLK"] + 0.5*NCAA[i, "AST"] - NCAA[i,"FGA"] - 0.5*NCAA[i,"FTA"] - 0.5*NCAA[i,"PF"] - NCAA[i,"TOV"])/NCAA[i,"MP"])
  NCAA[i, "WIN.SCORE"] <- win.score
}
for (i in 1:nrow(NBA)) {
  win.score <- as.double((NBA[i,"PTS"] + NBA[i, "TR"] + NBA[i,"ST"] + 0.5*NBA[i,"BK"] + 0.5*NBA[i, "AS"]  - NBA[i,"FGA"] - 0.5*NBA[i,"FTA"] - NBA[i,"TO"]- 0.5*NBA[i,"PF"])/NBA[i,"Min"])
  NBA[i, "WIN.SCORE"] <- win.score
}