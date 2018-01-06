
#### Exercises for Chapter 2 (p.55) ####

############################
### 1. Top Base Stealers ###
############################

names <- c('Rickey Henderson', 'Lou Brock', 'Ty Cobb', 'Eddie Collins', 'Max Carey', 
           'Joe Morgan', 'Luis Aparicio', 'Paul Molitor', 'Roberto Alomar')

# A. place SB CS and G in vectors 
SB <- c(1406, 938, 897, 741, 738, 689, 506, 504, 474) # Stolen Bases 
CS <- c(335, 307, 212, 195, 109, 162, 136, 131, 114) # Caught Stealing 
G <- c(3081, 2616, 3034, 2826, 2476, 2649, 2599, 2683, 2379) # Number of games 

# B. For all players, compute the number of stolen base attempts
SB.attempt <- SB + CS 

# C. For all players, compute the success rate 
Success.Rate <- SB / SB.attempt 

# D. Compute the number of stolen bases per game 
SB.Game <- SB / G 

# E. Construct a scatterplot of the stolen bases per game, against the success rate. 
plot(SB.Game, Success.Rate, 
     main="Stolen Bases per Game vs. Success Rate", 
     xlab="Stolen Bases per Game", 
     ylab="Success Rate")
text(SB.Game , Success.Rate, labels=names, cex=0.7, pos=1)
    # Max Carey has the highest success rate, Luis Aparicio and Eddie Collins have low ones 
    # Rickey Henderson has the greatest number of stolen bases per game 

### 2. Character, Factor and Logical Variables in R 

# a. Use the c function to collect outcomes in a character vector 
outcomes <- c('Single', 'Out', 'Out', 'Single', 'Out', 'Double', 'Out', 'Walk', 'Out', 'Single')

# b. Use the table function construct a frequency table of the outcomes 
table(outcomes)

# c. order the results from least-successful to most-successful 
f.outcomes <- factor(outcomes, levels=c('Out', 'Walk', 'Single', 'Double'))
table(f.outcomes)

# d. What is done in each of the following statements? 
outcomes == 'Walk' # Returns a boolean vector, for whatever position in 'outcomes' is equal to a Walk 
sum(outcomes == 'Walk') # Returns the number of elements in the vector that are Walks 


######################################## 
### 3. Pitchers in the 350-Wins club ### 
######################################## 

# a. Place the names, wins, and losses in vectors 
Name <- c('Alexander', 'Clemens', 'Galvin', 'Johnson', 'Maddux', 'Mathewson', 'Nichols', 
          'Spahn', 'Young')
W <- c(373, 354, 364, 417, 355, 373, 361, 363, 511) 
L <- c(208, 184, 310, 279, 227, 188, 208, 245, 316) 

# b. Calculate the winning percentages for all of the pitchers 
Win.PCT <- 100 * W/(W+L)

# c. Create a datafame with all the vectors 
Wins.350 <- data.frame(Name, W, L, Win.PCT)

# d. Sort the dataframe by winning percentage 
#Wins.350[with(Wins.350, order(Win.PCT)), ]
Wins.350[order(Wins.350$Win.PCT, decreasing=TRUE), ] 


###################################################
### 4. Pitchers in the 350-Wins Club, Continued ###
###################################################

# a. Place the SO and BB totals in vectors 
SO <- c(2198, 4672, 1806, 3509, 3371, 2502, 1868, 2583, 2803) 
BB <- c(951, 1580, 745, 1363, 999, 844, 1268, 1434, 1217) 

# b. Compute the strikeout to walk ratio 
SO.BB.Ratio <- SO / BB 

# c. Create a dataframe with the strikeouts and walks 
SO.BB <- data.frame(Name, SO, BB, SO.BB.Ratio) 

# d. Use the subset function, to find pitchers with a SO/BB ratios exceeding 2.8 
subset(SO.BB, SO.BB.Ratio > 2.8)

# e. Use the order function to sort the df by the number of walks 
SO.BB[order(SO.BB$BB, decreasing=TRUE), ]


########################################
### 5. Pitcher Strikeout/Walk Ratios ###
########################################

# a. Read the Lahman pitching CSV into a dataframe 
Pitching <- read.csv('pitching.csv') 

# b. Use the provided function to calculate career statistics for each pitcher in the dataframe 
stats <- function(d){
  c.SO <- sum(d$SO, na.rm=TRUE) 
  c.BB <- sum(d$BB, na.rm=TRUE) 
  c.IPouts <- sum(d$IPouts, na.rm=TRUE) 
  c.midYear <- median(d$yearID, na.rm=TRUE) 
  data.frame(Career.SO=c.SO, Career.BB=c.BB, Career.IPouts=c.IPouts, Career.midYear=c.midYear) 
}

library(plyr) 
career.pitching <- ddply(Pitching, .(playerID), stats)

# c. Merge the Pitching and career.pitching data frames 
Pitching <- merge(Pitching, career.pitching, by="playerID") 

# d. Use subset, to extract pitchers with at least 10,000 career IPouts (innings pitched, by outs .. IP * 3) 
career.10000 <- subset(Pitching, Pitching$Career.IPouts >= 10000) 

# e. For the pitchers with at least 10,000 IPouts, make a scatterplot of mid career year and ratior of 
# strikeouts to walks. 
career.10000$SO.BB <- career.10000$SO / career.10000$BB 
career.10000[1:5, c('SO', 'BB', 'SO.BB')] # See if it worked 

plot(career.10000$Career.midYear, career.10000$SO.BB, cex=0.4) 
#text(career.10000$Career.midYear , career.10000$SO.BB, labels=career.10000$playerID, pos=1) 

playernames <- read.csv('lahman/Master.csv') 
cols <- c('playerID', 'birthYear', 'nameFirst', 'nameLast')
playernames <- playernames[, cols]
career.10000 <- merge(career.10000, playernames, by="playerID")

