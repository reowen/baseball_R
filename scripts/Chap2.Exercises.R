
#### Exercises for Chapter 2 (p.55) ####

### 1. Top Base Stealers 

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

