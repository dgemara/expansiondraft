library(tidyverse)
library(lpSolve)


# Data --------------------------------------------------------------------

## CapFriendly, all available players available for expansion, no rookies

caphits <- read.csv("data/caphits.csv", stringsAsFactors = F) %>%
  mutate(
    Player =  toupper(Player),
    Player =  stringi::stri_trans_general(Player, "Latin-ASCII"),
    Player =
      gsub("NMC ", "", Player),
    Player =
      gsub(",", "", Player),
    n = str_count(Player, " ") + 1,
    first_name = word(Player, n),
    last_name = str_remove(Player, first_name) %>% trimws(),
    first_name_part = str_sub(first_name, 1, 3),
    FormattedPlayer = paste0(first_name_part, ".", last_name
    ) %>% toupper(),
    CAP.HIT = as.numeric(gsub('\\$|,', '', CAP.HIT))
    ) %>%
  select(-c(X, first_name, last_name, n, first_name_part))

rookies <- read.csv("data/rookies.csv", stringsAsFactors = F) %>%
  mutate(Player = EXEMPT,
         Player =  toupper(Player),
         Player =  stri_trans_general(Player, "Latin-ASCII"),
         Player =
           gsub("NMC ", "", Player),
         Player =
           gsub(",", "", Player),
         n = str_count(Player, " ") + 1,
         first_name = word(Player, n),
         first_name_part = str_sub(first_name, 1, 3),
         last_name = str_remove(Player, first_name) %>% trimws(),
         FormattedPlayer = paste0(first_name_part, ".", last_name
         ) %>% toupper(),
         CAP.HIT = as.numeric(gsub('\\$|,', '', CAP.HIT)),
         Rookie = 1
  ) %>%
  select(Player, FormattedPlayer, POS, AGE, CAP.HIT, EXPIRY, Rookie)

capfriendly <- bind_rows(rookies, caphits)


# correct JAY.MEGNA, NIC.JENSEN, COL.SISSONS, COL.WHITE, VIN.DUNN
# fix: BOO NIEVES

capfriendly$FormattedPlayer[capfriendly$Player == "MEGNA JAYSON"] <- "JAYSON.MEGNA"
capfriendly$FormattedPlayer[capfriendly$Player == "MEGNA JAYCOB"] <- "JAYCOB.MEGNA"
capfriendly$FormattedPlayer[capfriendly$Player == "JENSEN NICK"] <- "NICK.JENSEN"
capfriendly$FormattedPlayer[capfriendly$Player == "JENSEN NICKLAS"] <- "NICKLAS.JENSEN"
capfriendly$FormattedPlayer[capfriendly$Player == "SISSONS COLBY"] <- "COLBY.SISSONS"
capfriendly$FormattedPlayer[capfriendly$Player == "SISSONS COLTON"] <- "COLTON.SISSONS"
capfriendly$FormattedPlayer[capfriendly$Player == "WHITE COLTON"] <- "COLTON.WHITE"
capfriendly$FormattedPlayer[capfriendly$Player == "WHITE COLIN"] <- "COLIN.WHITE"
capfriendly$FormattedPlayer[capfriendly$Player == "DUNN VINCENT"] <- "VINCENT.DUNN"
capfriendly$FormattedPlayer[capfriendly$Player == "DUNN VINCE"] <- "VINCE.DUNN"

capfriendly %>%
  count(FormattedPlayer) %>%
  filter(n > 1)

## All players, from Corsica

gamescore <- read.csv("data/gamescore.csv", stringsAsFactors = F) %>%
  select(Player, GS, Player, Position) %>%
  mutate(
    Player = str_replace(Player, "ZACHARY.SANFORD", "ZACH.SANFORD"),
    Player = str_replace(Player, "J.T..COMPHER", "JT.COMPHER")
  ) %>%
  group_by(Player) %>%
  summarize(GS = sum(GS)) %>%
  mutate(
    last_name = str_extract(Player, "[^.]*$"),
    first_name = str_remove(Player, last_name),
    first_name_part = str_sub(first_name, 1, 3),
    FormattedPlayer = paste0(first_name_part, ".", last_name
    ) %>% toupper(),
    FormattedPlayer = str_replace(FormattedPlayer, "AJ..GREER", "AJ.GREER"),
    FormattedPlayer = str_replace(FormattedPlayer, "BO..HORVAT", "BO.HORVAT"),
    FormattedPlayer = str_replace(FormattedPlayer, "JT..COMPHER", "JT.COMPHER"),
    FormattedPlayer = str_replace(FormattedPlayer, "TJ..BRODIE", "TJ.BRODIE"),
    FormattedPlayer = str_replace(FormattedPlayer, "TJ..TYNAN", "TJ.TYNAN"),
    FormattedPlayer = str_replace(FormattedPlayer, "TY..RATTIE", "TY.RATTIE")
  ) %>%
  select(Player, FormattedPlayer, GS) 

gamescore$FormattedPlayer[gamescore$Player == "NICK.JENSEN"] <- "NICK.JENSEN"
gamescore$FormattedPlayer[gamescore$Player == "NICKLAS.JENSEN"] <- "NICKLAS.JENSEN"
gamescore$FormattedPlayer[gamescore$Player == "JAYCOB.MEGNA"] <- "JAYCOB.MEGNA"
gamescore$FormattedPlayer[gamescore$Player == "JAYSON.MEGNA"] <- "JAYSON.MEGNA"
gamescore$FormattedPlayer[gamescore$Player == "COLBY.SISSONS"] <- "COLBY.SISSONS"
gamescore$FormattedPlayer[gamescore$Player == "COLTON.SISSONS"] <- "COLTON.SISSONS"
gamescore$FormattedPlayer[gamescore$Player == "COLTON.WHITE"] <- "COLTON.WHITE"
gamescore$FormattedPlayer[gamescore$Player == "COLIN.WHITE"] <- "COLIN.WHITE"
gamescore$FormattedPlayer[gamescore$Player == "VINCENT.DUNN"] <- "VINCENT.DUNN"
gamescore$FormattedPlayer[gamescore$Player == "VINCE.DUNN"] <- "VINCE.DUNN"



## Protected players, from Wikipedia
protected <- read.csv("data/protected.csv", stringsAsFactors = F) %>%
  filter(value != "") %>%
  mutate(
    Player = value,
    Team = key,
    first_name = word(Player, 1),
    first_name_part = str_sub(first_name, 1, 3),
    last_name = str_remove(Player, first_name) %>% trimws(),
    FormattedPlayer = paste0(first_name_part, ".", last_name) %>% toupper(),
    Protected = 1,
    FormattedPlayer = str_replace(FormattedPlayer, "P..K. SUBBAN", "P.K.SUBBAN")
    ) %>%
  select(FormattedPlayer, Protected) 

# All players, non-goalies, hockey reference
hockey_reference <- 
  read.csv("data/pointshares.csv", stringsAsFactors = F) %>%
  mutate(
    first_name = word(Player, 1),
    first_name_part = str_sub(first_name, 1, 3),
    last_name = str_remove(Player, first_name) %>% trimws(), 
    FormattedPlayer = paste0(first_name_part, ".", last_name
    ) %>% toupper(),
    FormattedPlayer = str_replace(FormattedPlayer, "BOO.NIEVES", "CRI.NIEVES"),
    FormattedPlayer = str_replace(FormattedPlayer, "JOS.MORROW", "JOE.MORROW"),
    FormattedPlayer = str_replace(FormattedPlayer, "MIK.MATHESON", "MIC.MATHESON")
  ) %>%
  group_by(Player, FormattedPlayer, Pos, TEAM) %>%
  summarize(PS = sum(PS)) %>%
  ungroup()

hockey_reference$FormattedPlayer[hockey_reference$Player == "Jayson Megna"] <- "JAYSON.MEGNA"
hockey_reference$FormattedPlayer[hockey_reference$Player == "Jaycob Megna"] <- "JAYCOB.MEGNA"
hockey_reference$FormattedPlayer[hockey_reference$Player == "Nick Jensen"] <- "NICK.JENSEN"
hockey_reference$FormattedPlayer[hockey_reference$Player == "Nicklas Jensen"] <- "NICKLAS.JENSEN"
hockey_reference$FormattedPlayer[hockey_reference$Player == "Colin White"] <- "COLIN.WHITE"
hockey_reference$FormattedPlayer[hockey_reference$Player == "Colton Sissons"] <- "COLTON.SISSONS"
hockey_reference$FormattedPlayer[hockey_reference$Player == "J.T. Compher"] <- "JT.COMPHER"
hockey_reference$FormattedPlayer[hockey_reference$Player == "A.J. Greer"] <- "AJ.GREER"
hockey_reference$FormattedPlayer[hockey_reference$Player == "T.J. Brodie"] <- "TJ.BRODIE"
hockey_reference$FormattedPlayer[hockey_reference$Player == "T.J. Tynan"] <- "TJ.TYNAN"


goalies <- read.csv("data/goalies.csv", stringsAsFactors = F) %>%
  mutate(
    GS = 0.075 * SV - 0.75 * GA - 0.07 * GP,
    first_name = word(Player, 1),
    first_name_part = str_sub(first_name, 1, 3),
    last_name = str_remove(Player, first_name) %>% trimws(), 
    FormattedPlayer = paste0(first_name_part, ".", last_name
    ) %>% toupper()
  ) %>%
  select(Player, FormattedPlayer, GS) %>%
  left_join(select(hockey_reference, -Player), by = "FormattedPlayer")

names(goalies)

non_goalies <- hockey_reference %>%
  filter(Pos != "G") %>%
  left_join(select(gamescore, -Player), by = "FormattedPlayer") 

expansion <- goalies %>%
  bind_rows(non_goalies) %>%
  left_join(capfriendly, by = "FormattedPlayer") %>%
  left_join(protected, by = "FormattedPlayer") %>%
  filter(!is.na(Player.y)) %>%
  mutate(Rookie = replace_na(Rookie,0),
         Protected = replace_na(Protected,0),
         Player = Player.x) %>%
  filter(
    Rookie == 0,
    Protected == 0,
    EXPIRY != "UFA (2017)"
  ) %>%
  select(-c(Player.x, Player.y)) %>%
  mutate(
    scaled_GS = scale(GS),
    scaled_PS = scale(PS)
  ) 
  

table(expansion$POS)
table(expansion$TEAM)


# Model -------------------------------------------------------------------

i = nrow(expansion)
cons = rbind(
  rep(1, i),
  # sapply(expansion$Pos, function(x)
  #   if (x == "LW")
  #     x = 1
  #   else
  #     x = 0),
  # # (2) left wingers
  # sapply(expansion$Pos, function(x)
  #   if (x == "RW")
  #     x = 1
  #   else
  #     x = 0),
  # # (3) right wingers
  # sapply(expansion$Pos, function(x)
  #   if (x == "C")
  #     x = 1
  #   else
  #     x = 0),
  # # (4) centers
  sapply(expansion$Pos, function(x)
    if (x %in% c("C", "LW", "RW"))
      x = 1
    else
      x = 0),
  # (4) centers
  sapply(expansion$Pos, function(x)
    if (x == "D")
      x = 1
    else
      x = 0),
  # (5) defensemen
  sapply(expansion$Pos, function(x)
    if (x == "G")
      x = 1
    else
      x = 0),
  # (6) goalies
  
  
  sapply(expansion$TEAM, function(x)
    if (x == "CAR")
      x = 1
    else
      x = 0),
  # (7) #represenative from each team
  sapply(expansion$TEAM, function(x)
    if (x == "CBJ")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "NJD")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "NYI")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "NYR")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "WSH")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "PHI")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "PIT")
      x = 1
    else
      x = 0),
  # (7)
  #
  sapply(expansion$TEAM, function(x)
    if (x == "ANA")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "ARI")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "CGY")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "EDM")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "LAK")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "VAN")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "SJS")
      x = 1
    else
      x = 0),
  # (7)
  #
  sapply(expansion$TEAM, function(x)
    if (x == "STL")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "CHI")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "COL")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "WPG")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "MIN")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "NSH")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "DAL")
      x = 1
    else
      x = 0),
  # (7)
  #
  sapply(expansion$TEAM, function(x)
    if (x == "DET")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "BOS")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "BUF")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "FLA")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "TBL")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "TOR")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "MTL")
      x = 1
    else
      x = 0),
  # (7)
  sapply(expansion$TEAM, function(x)
    if (x == "OTT")
      x = 1
    else
      x = 0),
  # (7)
  
  expansion$CAP.HIT,
  expansion$CAP.HIT # (9) salary cap
  #
)

## Game Score
f.obj = apply((expansion$scaled_GS), 1, mean)
f.dir = c("=", rep(">=", 3), rep("=", 30), ">=", "<=") 
f.rhs = c(30, 14, 9, 3, rep(1, 30), 43.8*10^6, 73*10^6)

model = lp("max",
           f.obj,
           cons,
           f.dir,
           f.rhs,
           all.bin = T,
           compute.sens = 1)
model

sol = model$solution
sol

game_score = expansion[which(sol>0),c(12, 6,4, 2, 5, 8)]

game_score %>%
  arrange(TEAM)


## Point Shares
f.obj = apply((expansion$scaled_PS), 1, mean)
f.dir = c("=", rep(">=", 3), rep("=", 30), ">=", "<=") 
f.rhs = c(30, 14, 9, 3, rep(1, 30), 43.8*10^6, 73*10^6)

model = lp("max",
           f.obj,
           cons,
           f.dir,
           f.rhs,
           all.bin = T,
           compute.sens = 1)
model

sol = model$solution
sol

point_shares = expansion[which(sol>0),c(12, 6,4, 2, 5, 8)]

point_shares %>%
  arrange(TEAM)

## Point shares are skewing results towards goalies (based on time on ice, may need to adjust point shares)

## Future steps: all star, need to deal with either-or constraints for all star game captains

## Code is based off of http://www.kobakhit.com/intermediate/2016/02/18/linear-opt-baseball.html