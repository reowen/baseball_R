#####################################################
# Chapter 5:  Value of Plays Using Run Expectancy
#
# datafiles:  all2001.csv, fields.csv, roster2011.csv
# R packages:  plyr, MASS
#
#####################################################

##################################################
# 5.2  Runs Scored in the Remainder of the Inning
##################################################
 
##################################################
# 5.4  Measuring Success of a Batting Play
##################################################

# After a batting play, the "state" changes (runners on, num outs). The value of a batting play, is the 
# difference between the change in expected runs by end of inning, and the actual runs produced by the play. 

RUNS.POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), 32, 1) # creates a 32x1 matrix (merge-able)
dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$Group, "000 3","001 3",
                                   "010 3","011 3","100 3","101 3","110 3","111 3") 
data2011$RUNS.STATE <- RUNS.POTENTIAL[data2011$STATE,] # merges the expected values
data2011$RUNS.NEW.STATE <- RUNS.POTENTIAL[data2011$NEW.STATE,] # runs expectancy post-batting play 
data2011$RUNS.VALUE <- data2011$RUNS.NEW.STATE - data2011$RUNS.STATE + 
  data2011$RUNS.SCORED

##################################################
# 5.5  Albert Pujols
##################################################

Roster <- read.csv("roster2011.csv")
albert.id <- subset(Roster, First.Name == "Albert" &
                     Last.Name == "Pujols")$Player.ID
albert.id <- as.character(albert.id)
albert <- subset(data2011, BAT_ID==albert.id)
albert <- subset(albert, BAT_EVENT_FL==TRUE) # isolate events where Pujols was the hitter 

albert[1:2, c("STATE", "NEW.STATE", "RUNS.VALUE")] # how were the first two plate appearances? 

# Tabulate how often Pujols was batting with runners in scoring position, etc...
albert$RUNNERS <- substr(albert$STATE, 1, 3)
table(albert$RUNNERS)

# Visualize how Pujols performed with runners in each of these states 
with(albert, stripchart(RUNS.VALUE ~ RUNNERS, vertical=TRUE, jitter=0.2,
                        xlab="RUNNERS", method="jitter", pch=1, cex = 0.8))
abline(h=0)

# Compute the number of opportunities, and sum of runs, for Pujols in each of the states. 
A.runs <- aggregate(albert$RUNS.VALUE, list(albert$RUNNERS), sum)
names(A.runs)[2] <- "RUNS"
A.PA <- aggregate(albert$RUNS.VALUE, list(albert$RUNNERS), length)
names(A.PA)[2] <- "PA"
A <- merge(A.PA, A.runs)
A

# Calculate the total runs contribution for the season 
sum(A$RUNS) # 27 doesn't mean much with no context.. so we'll compare this to how the other players did 

##################################################
# 5.6  Opportunity and Success for All Hitters
##################################################

data2011b <- subset(data2011, BAT_EVENT_FL == TRUE)

# Calculate total plate appearances, run value, and run potential for each player in 2011 
runs.sums <- aggregate(data2011b$RUNS.VALUE, list(data2011b$BAT_ID), sum)
runs.pa <- aggregate(data2011b$RUNS.VALUE, list(data2011b$BAT_ID), length)
runs.start <- aggregate(data2011b$RUNS.STATE, list(data2011b$BAT_ID), sum)
names(runs.sums) <- c("Batter", "Runs")
names(runs.pa) <- c("Batter", "PA")
names(runs.start) <- c("Batter", "Runs.Start")
runs <- merge(runs.sums, runs.pa)
runs <- merge(runs, runs.start)

# We don't want pitchers, or non-starters, so we restrict to batters with >=400 plate appearances 
runs400 <- subset(runs, PA >=400)
head(runs400)

# Scatterplot of run opportunities, against run value: as expected, 
# batters with more run opportunities produce higher value
with(runs400, plot(Runs.Start, Runs))
with(runs400, lines(lowess(Runs.Start, Runs)))
abline(h=0)

# Restrict to only the batters who created more than 40 runs 
runs400.top <- subset(runs400, Runs >=40)
roster2011 <- read.csv("roster2011.csv")  
runs400.top <- merge(runs400.top, 
              roster2011, by.x="Batter", by.y="Player.ID")
with(runs400.top, text(Runs.Start, Runs, Last.Name, pos=1)) # label those points on the plot 

##################################################
# 5.7  Position in the Batting Lineup
##################################################

# Find the batting position they had the most frequently, for each player 
get.batting.pos <- function(batter){
  TB <- table(subset(data2011,BAT_ID==batter)$BAT_LINEUP_ID)
  names(TB)[TB==max(TB)][1]}

position <- sapply(as.character(runs400$Batter), get.batting.pos)

# Plot the run values, against potential, using batting position 
with(runs400, plot(Runs.Start, Runs, type="n")) # type="n" plots just the axes, without point (we use text() below)
with(runs400, lines(lowess(Runs.Start, Runs)))
abline(h=0)
with(runs400, text(Runs.Start, Runs, position)) # this plots the batting position, in lieu of points 

# How does Pujols stack up in this scatterplot? 
AP <- subset(runs400, Batter==albert.id)
points(AP$Runs.Start, AP$Runs, pch=19, cex=3)

##################################################
# 5.8 Run Values of Different Base Hits
##################################################

#### Calculate run value of a home run ####
d.homerun <- subset(data2011, EVENT_CD==23) # dataset with only the homeruns 

table(d.homerun$STATE) # tabulate runner states for each HR 

round(prop.table(table(d.homerun$STATE)), 3) # display relative frequencies, rounded to 3 decimal places 
# over half of HRs are hit with no runners on base 

# Histogram of run values, for all HRs 
library(MASS)
truehist(d.homerun$RUNS.VALUE) # most homeruns have run value of 1 (i.e. bases empty)

# Which runners/out situations lead to the most valuable homeruns? 
# Extract the play with the largest run value 
subset(d.homerun, RUNS.VALUE==max(RUNS.VALUE))[1, 
      c("STATE", "NEW.STATE", "RUNS.VALUE")] # bases loaded with 2 outs 

# Overall run value of a homerun: the mean run value 
mean.HR <- mean(d.homerun$RUNS.VALUE)
mean.HR

abline(v = mean.HR, lwd=3) # draw the mean value on the histogram 
text(1.5, 5, "Mean Runs Value", pos=4)

#### Calculate the run value of a single #### 
d.single <- subset(data2011, EVENT_CD == 20)
#library(MASS)
truehist(d.single$RUNS.VALUE)

table(d.single$STATE) # most singles occur with the bases empty 

subset(d.single, d.single$RUNS.VALUE==
  max(d.single$RUNS.VALUE))[, c("STATE", "NEW.STATE", "RUNS.VALUE")]

subset(d.single, d.single$RUNS.VALUE == min(d.single$RUNS.VALUE))[
  , c("STATE", "NEW.STATE", "RUNS.VALUE")]

mean.single <- mean(d.single$RUNS.VALUE)
mean.single
abline(v = mean.single, lwd=3)
text(.5, 5, "Mean Runs Value", pos=4)

##################################################
# 5.9  Value of Base Stealing
##################################################

stealing <- subset(data2011, EVENT_CD==6 | EVENT_CD==4) # 6 == "Caught Stealing", 4 == "Stolen base" 

table(stealing$EVENT_CD)

table(stealing$STATE) # most common runner/out states for attempting a stolen base? 

#library(MASS)
truehist(stealing$RUNS.VALUE)

# Isolate to situations with 1 out, runner on first 
stealing.1001 <- subset(stealing, STATE=="100 1")

table(stealing.1001$EVENT_CD)

with(stealing.1001, table(NEW.STATE))

mean(stealing.1001$RUNS.VALUE) # stolen bases are worthwhile, though the effect is minimal 

#######################################################





