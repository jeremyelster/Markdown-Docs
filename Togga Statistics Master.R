# Bring in correct packages
list.of.packages <- c("dplyr", "ggvis", "DT", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr)
library(ggvis)
library(DT)
library(data.table)

# Set working directory (may need to change)
setwd("~/Documents/R Projects/Togga Fantasy Football/Week 1-4")

# Call in correct csv and give name and convert to data.table
allMatchPoints <- read.csv("All Results Edited.csv")
allMatchPoints <- as.data.table(allMatchPoints)

# Converts Togga Data to Fantasy Points for Each Player
# Convert Type to Double
allFantasyPoints <- allMatchPoints
allFantasyPoints[, 
                 c(9:27) := list(
                   as.double(PTS),
                   as.double(G),
                   as.double(A),
                   as.double(SOT),
                   as.double(CC),
                   as.double(SCR),
                   as.double(STO),
                   as.double(CS),
                   as.double(INT),
                   as.double(TW),
                   as.double(PS),
                   as.double(SV),
                   as.double(AER),
                   as.double(CLR),
                   as.double(DIS),
                   as.double(OG),
                   as.double(GC),
                   as.double(YC),
                   as.double(RC))]
  
allFantasyPoints[allFantasyPoints$Position == "For",
                 c(10:27) := list(
                   G*9,
                   A*6,
                   SOT*2,
                   CC*3,
                   SCR,
                   STO,
                   CS*0,
                   INT,
                   TW,
                   PS*0,
                   SV*0,
                   AER*.5,
                   CLR*0,
                   -DIS,
                   -OG*9,
                   GC*0,
                   -YC*3,
                   -RC*7)]
allFantasyPoints[allFantasyPoints$Position == "Mid",
                 c(10:27) := list(
                   G*9,
                   A*6,
                   SOT*2,
                   CC*3,
                   SCR,
                   STO,
                   CS,
                   INT,
                   TW,
                   PS*0,
                   SV*0,
                   AER*.5,
                   CLR*0,
                   -DIS,
                   -OG*9,
                   GC*0,
                   -YC*3,
                   -RC*7)]
allFantasyPoints[allFantasyPoints$Position == "Def",
                 c(10:27) := list(
                   G*10,
                   A*8,
                   SOT*2,
                   CC*3,
                   SCR,
                   STO,
                   CS*6,
                   INT,
                   TW,
                   PS*0,
                   SV*0,
                   AER,
                   CLR*.25,
                   -DIS,
                   -OG*9,
                   -GC*2,
                   -YC*3,
                   -RC*7)]
allFantasyPoints[allFantasyPoints$Position == "Goa",
                 c(10:27) := list(
                   G*10,
                   A*8,
                   SOT*2,
                   CC*6,
                   SCR,
                   STO,
                   CS*8,
                   INT,
                   TW,
                   PS*8,
                   SV*2,
                   AER,
                   CLR*.25,
                   -DIS,
                   -OG*9,
                   -GC*2,
                   -YC*3,
                   -RC*7)]

# Converts Togga Data to Fantasy Points for Each Player (Totals)
allPlayerTotals <- allFantasyPoints[,lapply(.SD, sum), by = .(Team, Player, Position), .SDcols = c(9:27)]

# Converts Togga Data to Fantasy Points for Each Player (Totals)
allTeamTotals <- allFantasyPoints[,lapply(.SD, sum), by = Team, .SDcols = c(9:27)]
tests <- allFantasyPoints
tests[, MP := max(GW)]
tests[, Wins := sum(Result == "W"), by = Team]



# Converts Togga Data to Fantasy Category Points for Each Player
fantasyPlayerCategory <- allPlayerTotals
fantasyPlayerCategory <- fantasyPlayerCategory[, 
                         .(Points = PTS,
                           OffPoints = G + A + SOT + CC + SCR + STO + DIS,
                           DefPoints = INT + TW + AER + OG + CS + CLR + GC,
                           GKPoints = PS + SV,
                           Discipline = YC + RC),
                         by = .(Player, Team, Position)][order(-Points)]
fantasyPlayerCategory

# Converts Togga Data to Fantasy Category Points for Each Team
fantasyTeamCategory <- allTeamTotals
fantasyTeamCategory <- fantasyTeamCategory[, 
                                               .(Points = PTS,
                                                 OffPoints = G + A + SOT + CC + SCR + STO + DIS,
                                                 DefPoints = INT + TW + AER + OG + CS + CLR + GC,
                                                 GKPoints = PS + SV,
                                                 Discipline = YC + RC),
                                               by = Team][order(-Points)]
fantasyTeamCategory

# Every Gameweek Summed by Team
allGameweekTotals <- allFantasyPoints[,lapply(.SD, sum), by = .(Team, GW, Home.Away, Result, OPP, Score), .SDcols = c(9:27)]

# Week to Week by Team Scores
teamWeekTotals <- function(team) {
  allFantasyPoints[allFantasyPoints$Team == team, lapply(.SD, sum), by = .(Team, GW, Home.Away, Result, Score), .SDcols = c(9:27)]
}

# Output Team, Week, Opponent, Result, Home/Away, Score, and Fantasy Points
week_result <- function(club, week) {
  allMatchResults[allMatchResults$Team == club & allMatchResults$GW == week]
}



# Fun
boxplot(allGameweekTotals$PTS ~ allGameweekTotals$Home.Away)
boxplot(allGameweekTotals$PTS ~ allGameweekTotals$Result)
