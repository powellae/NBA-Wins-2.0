#NBA Wins 2.0 -- Data Cleaning
#Alexander Powell
#Updated: August 21, 2018

#Packages
library(dplyr)
devtools::install_github("r-lib/progress")
library(progress)

#Basketball-Reference Data
bref <- read.csv("bref.csv", stringsAsFactors = FALSE)

#Remove unwanted and duplicate columns
bref$X <- NULL
bref$X.1 <- NULL
bref$Player.1 <- NULL
bref$Pos.1 <- NULL
bref$Age.1 <- NULL
bref$Tm.1 <- NULL
bref$G.1 <- NULL
bref$GS <- NULL
bref$X.2 <- NULL

#Reduce trade seasons (partial rows)
trade <- bref %>%
  filter(Tm == "TOT")
remove.rows <- c()

for(i in 1:nrow(bref)){
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
