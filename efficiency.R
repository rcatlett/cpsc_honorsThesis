# EFF = (PTS + REB + AST + STL + BLK − Missed FG − Missed FT − TO) / GP

#efficiencyNBA
#for (i in 1:nrow(NBA)) {
#  eff <- as.double((NBA[i,"PTS"] + NBA[i,"TR"] + NBA[i,"AS"] + NBA[i,"ST"] + NBA[i,"BK"] - (NBA[i, "FGA"]-NBA[i, "FGM"]) - (NBA[i,"FTA"]- NBA[i,"FTM"]) - NBA[i,"TO"])/NBA[i,"GP"])
#  NBA[i, "EFF"] <- eff
#}

# eficiency NCAA
for (i in 1:nrow(NCAA)) {
  eff <- as.double((NCAA[i,"PTS"] + NCAA[i,"TRB"] + NCAA[i,"AST"] + NCAA[i,"STL"] + NCAA[i,"BLK"] - (NCAA[i, "FGA"]-NCAA[i, "FG"]) - (NCAA[i,"FTA"]- NCAA[i,"FT"]) - NCAA[i,"TOV"])/NCAA[i,"G"])
  NCAA[i, "EFF"] <- eff
}