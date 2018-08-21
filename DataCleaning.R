#NBA Wins 2.0 -- Data Cleaning
#Alexander Powell
#Updated: August 21, 2018

#Packages
library(dplyr)

#Basketball-Reference Data
bref <- read.csv("bref.csv")

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
  if(i %in% values){
    print(i)
  }
  if(bref$Tm[i] != "TOT"){
    tmp <- trade %>%
      filter(Year == bref$Year[i])
  
    if(bref$Player[i] %in% tmp$Player){
      remove.rows <- c(remove.rows, i)
    }
  }
}

PlayerValues <- bref[-remove.rows,]

#Remove bref username from player name
