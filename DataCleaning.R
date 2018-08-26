#NBA Wins 2.0 -- Data Cleaning
#Alexander Powell
#August 21-25, 2018

#Packages
library(dplyr)
devtools::install_github("r-lib/progress")
library(progress)

#Basketball-Reference Data
bref <- read.csv("bref.csv", stringsAsFactors = FALSE)
height <- read.csv("height.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(Player, Ht)

#Remove unwanted and duplicate columns
bref <- merge(bref, height, by = "Player", all.x = TRUE)
bref$X <- NULL
bref$X.1 <- NULL
bref$Player.1 <- NULL
bref$Pos.1 <- NULL
bref$Age.1 <- NULL
bref$Tm.1 <- NULL
bref$G.1 <- NULL
bref$GS <- NULL
bref$X.2 <- NULL

#Reduce trade seasons (partial rows) & fix height (to inches)
trade <- bref %>%
  filter(Tm == "TOT")
remove.rows <- c()

for(i in 1:nrow(bref)){
  feet <- gsub("-.*", "", bref$Ht[i])
  inches <- gsub("*.-", "", bref$Ht[i])
  bref$Ht[i] <- as.numeric(feet)*12 + as.numeric(inches)
  
  if(bref$Tm[i] != "TOT"){
    tmp <- trade %>%
      filter(Year == bref$Year[i])
  
    if(bref$Player[i] %in% tmp$Player){
      remove.rows <- c(remove.rows, i)
    }
  }
}

PlayerValues <- bref[-remove.rows,]
rownames(PlayerValues) <- 1:nrow(PlayerValues)

#Remove bref username from player name
PlayerValues$Player <- gsub("\\\\.*", "", PlayerValues$Player)
PlayerValues$Player <- gsub("\\*.*", "", PlayerValues$Player)

#Add RPM/BPM (via ESPN & Bref) by season
BPM <- PlayerValues %>%
  filter(MP >= 300) %>%
  filter(Year < 2014) %>%
  dplyr::select(Year, Player, BPM)

RPM <- read.csv("PlayerRPM.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(Year, NAME, RPM)

colnames(BPM) <- c("Year", "Player", "RPM")
colnames(RPM) <- c("Year", "Player", "RPM")

RPM <- rbind(RPM, BPM)

#Adding future 3 RPM values to PlayerValues df
pb <- progress_bar$new(total = nrow(PlayerValues))
for(i in 1:nrow(PlayerValues)){
  pb$tick()
  Sys.sleep(1/nrow(PlayerValues))
  
  year <- PlayerValues$Year[i]
  
  temp <- RPM %>%
    filter(Year == (year+1) | Year == (year+2) | Year == (year+3)) %>%
    filter(Player == PlayerValues$Player[i])
  
  if(nrow(temp) == 0){
    PlayerValues$RPM1[i] <- NA
    PlayerValues$RPM2[i] <- NA
    PlayerValues$RPM3[i] <- NA
  } else {
    for(j in 1:nrow(temp)){
      if(temp$Year[j] == (year+1)){
        PlayerValues$RPM1[i] <- temp$RPM[j]
      } else if(temp$Year[j] == (year+2)){
        PlayerValues$RPM2[i] <- temp$RPM[j]
      } else if(temp$Year[j] == (year+3)){
        PlayerValues$RPM3[i] <- temp$RPM[j]
      }
    }
  }
}

#Weights
PlayerValues <- PlayerValues %>%
  dplyr::select(Player, Year, RPM1, RPM2, RPM3, Pos, Age, Ht, G, MP, TS., X3PAr, FTr, ORB., DRB., TRB., AST., STL., BLK., TOV., USG.,
                OBPM, DBPM, FG, FGA, FG., X3P, X3PA, X3P., X2P, X2PA, X2P., FT, FTA, FT., PF, PTS, ORtg, DRtg) %>%
  filter(MP >= 300)

PlayerValues$X3P.[is.na(PlayerValues$X3P.)] <- 0
PlayerValues$X2P.[is.na(PlayerValues$X2P.)] <- 0
PlayerValues$FT.[is.na(PlayerValues$FT.)] <- 0

pb <- progress_bar$new(total = nrow(PlayerValues))
WeightedPlayers <- data.frame()
for(i in 1:nrow(PlayerValues)){
  pb$tick()
  Sys.sleep(1/nrow(PlayerValues))
  
  temp <- PlayerValues %>%
    filter(Player == PlayerValues$Player[i]) %>%
    filter(Year == (PlayerValues$Year[i]) | Year == (PlayerValues$Year[i] - 1) | Year == (PlayerValues$Year[i] - 2))
  
  allthree <- 0
  recent_row <- 0
  for(j in 1:nrow(temp)){
    if(is.na(temp$RPM3[j])){
      allthree <- 1
    }
    if(temp$Year[j] == PlayerValues$Year[i]){
      temp[j,9:39] <- temp[j,9:39]*1
      recent_row <- j
    } else if(temp$Year[j] == (PlayerValues$Year[i]-1)){
      temp[j,9:39] <- temp[j,9:39]*2
    } else if(temp$Year[j] == (PlayerValues$Year[i]-2)){
      temp[j,9:39] <- temp[j,9:39]*3
    }
  }
  
  if(allthree == 0 & nrow(temp) > 3){
    print(PlayerValues$Player[i])
  } else if(allthree == 0 & nrow(temp) <= 3){
    player_row <- temp[recent_row,1:8]
    #stats_row <- ifelse(nrow(temp)==3, temp[1,9:39] + temp[2,9:39] + temp[3,9:39], ifelse(nrow(temp)==2, temp[1,9:39] + temp[2,9:39], temp[1,9:39]))
    if(nrow(temp) == 3){
      stats_row <- temp[1,9:39] + temp[2,9:39] + temp[3,9:39]
    } else if(nrow(temp) == 2){
      stats_row <- temp[1,9:39] + temp[2,9:39]
    } else {
      stats_row <- temp[1,9:39]
    }
    divisor <- ifelse(nrow(temp) == 3, 6, ifelse(nrow(temp) == 2, 3, 1))
    stats_row <- stats_row / divisor
    player_row <- cbind(player_row, stats_row)
    WeightedPlayers <- rbind(WeightedPlayers, player_row)
  }
  
}

WeightedPlayers$X2P. <- WeightedPlayers$X2P / WeightedPlayers$X2PA
WeightedPlayers$X3P. <- WeightedPlayers$X3P / WeightedPlayers$X3PA
WeightedPlayers$FT. <- WeightedPlayers$FT / WeightedPlayers$FTA
