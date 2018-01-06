
#### Exercises for Chapter 3 (p.83) #### 

# First, try creating your own hof batters list 
players <- read.csv('lahman/Master.csv') 

hof_all <- read.csv('lahman/HallOfFame.csv') 
hoflist <- hof_all[hof_all$category=='Player' & hof_all$inducted=='Y', 'playerID'] 
pitch <- read.csv('pitching.csv')
pitchers <- unique(pitch$playerID) 
hof_batters <- unique(hoflist[!(hoflist %in% pitchers)])
paste('There are', length(hof_batters), 'batters in the hall of fame.')

########################################
### 1. Hall of Fame Pitching Dataset ###
########################################


