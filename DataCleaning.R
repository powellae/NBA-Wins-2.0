#NBA Wins 2.0 -- Data Cleaning
#Alexander Powell
#Updated: August 21, 2018

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
