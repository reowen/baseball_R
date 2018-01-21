
library(plyr)
# Import all of the objects from the Chapter 5 notes 
source('../scripts/Chap5.runsexpectancy.R')

###########################
## 1. Run Values of Hits ##
###########################

## A. Find the mean run values for a double and a triple 

# Double 
d.dbl <- subset(data2011, EVENT_CD==21) 

table(d.dbl$STATE) 

round(prop.table(table(d.dbl$STATE)), 3) 

library(MASS)
truehist(d.dbl$RUNS.VALUE) 

# Which runners/out situations lead to the most valuable doubles? 
subset(d.dbl, RUNS.VALUE==max(RUNS.VALUE))[1, c("STATE", "NEW.STATE", "RUNS.VALUE")] 

# Overall run value of a double: the mean run value 
mean.DBL <- mean(d.dbl$RUNS.VALUE)
mean.DBL 

abline(v = mean.DBL, lwd=3) # draw the mean value on the histogram 
text(1.5, 5, "Mean Runs Value", pos=4) 


# Triple 
d.tpl <- subset(data2011, EVENT_CD==22) 

table(d.tpl$STATE) 

round(prop.table(table(d.tpl$STATE)), 3) 

truehist(d.tpl$RUNS.VALUE) 

# Which runners/out situations lead to the most valuable triples? 
subset(d.tpl, RUNS.VALUE==max(RUNS.VALUE))[1, c("STATE", "NEW.STATE", "RUNS.VALUE")]  

# Overall run value of a triple: the mean run value 
mean.TPL <- mean(d.tpl$RUNS.VALUE)
mean.TPL 

abline(v = mean.TPL, lwd=3) # draw the mean value on the histogram 
text(1.5, 5, "Mean Runs Value", pos=4) 


## B. Compare the regression weights (coefficients) to the mean run values 
# re-calculate the HR and single values 
d.homerun <- subset(data2011, EVENT_CD==23) # dataset with only the homeruns  
mean.HR <- mean(d.homerun$RUNS.VALUE) 

d.single <- subset(data2011, EVENT_CD == 20) 
mean.single <- mean(d.single$RUNS.VALUE)  

means <- round(c(mean.single, mean.DBL, mean.TPL, mean.HR), 2)
regweights <- c(0.46, 0.80, 1.02, 1.40) 
diff <- round(means - regweights, 2)
results <- matrix(c(means, regweights, diff), 4, 3) 
dimnames(results)[[1]] <- c("Single", "Double", "Triple", "Home Run")
dimnames(results)[[2]] <- c("Means", "Reg Weights", "Difference") 
results 


#######################################################
## 2. Value of Different Ways of Reaching First Base ##
#######################################################

## Use run values to compare the value of a single, walk, and hit-by-pitch when there is a single runner on first base 
# single: EVENT_CD == 20; walk: EVENT_CD == 14; HBP: EVENT_CD == 16 

genMean <- function(cd, pos){
  mu <- mean(subset(data2011, EVENT_CD == cd & substr(STATE, 1, 3) == pos)$RUNS.VALUE)
  return(round(mu, 2))
}
mean.single <- genMean(20, "100")
mean.BB <- genMean(14, "100")
mean.HBP <- genMean(16, "100")
results <- matrix(c(mean.single, mean.BB, mean.HBP), 3, 1) 
dimnames(results)[[1]] <- c("Single", "Walk", "Hit by Pitch") 
results


################################################
## 3. Comparing Two Players with Similar OBPs ##
################################################

genPlayerDF <- function(id){
  df <- subset(data2011, BAT_ID == id & BAT_EVENT_FL == TRUE) 
  df$RUNNERS <- substr(df$STATE, 1, 3) 
  
  agg <- ddply(df, 'RUNNERS', summarize, 
               RUNS = sum(RUNS.VALUE), 
               PA = length(RUNS.VALUE))
  return(df)}

## Compare the run values of Rickie Weeks ("weekr001") and Michael Bourne ("bourm001") 
## Which player was more valuable to his team? 

weeks <- genPlayerDF("weekr001")
bourne <- genPlayerDF("bourm001") 

sum(weeks$RUNS)
sum(bourne$RUNS)

## Can you explain the difference in values in terms of traditional batting stats such as AVG, SLG, or OBP? 
library(Lahman) 
batting <- subset(battingStats(), yearID == 2011 & playerID %in% c('weeksri01', 'bournmi01'))  
batting2 <- ddply(batting, 'playerID', summarize, 
                 H = sum(H), 
                 AB = sum(AB), 
                 DBLS = sum(X2B), 
                 TPLS = sum(X3B), 
                 HR = sum(HR), 
                 BB = sum(BB), 
                 HBP = sum(HBP), 
                 SF = sum(SF), OBP_x=sum(OBP), SlugPct = sum(SlugPct))

batting2$BA <- with(batting2, round(H / AB, 3))
batting2$SLG <- with(batting2, round((H + DBLS + 2*TPLS + 3*HR) / AB, 3))
batting2$OBP <- with(batting2, round((H + BB + HBP) / (AB + BB + HBP + SF), 3))

batting2$RV <- with(batting2, ifelse(playerID == 'bournmi01', sum(bourne$RUNS), sum(weeks$RUNS))) 

display_cols <- c('playerID', 'BA', 'SLG', 'OBP', 'RV') 
batting2[, display_cols] # Weeks has a higher run value, due to his higher slugging percentage 


###################################################
## 4. Create Probability of Scoring a Run Matrix ##
###################################################

## Compute the proportion of times that at least one run was scored, for each of the 24 possible position/out states 

data2011b <- subset(data2011, BAT_EVENT_FL == TRUE) 
data2011b$RUNS_F <- data2011b$RUNS.SCORED > 0 

runs <- ddply(data2011b, 'STATE', summarize, 
              RUNS_PR = round(mean(RUNS_F), 3))
runs$Outs <- substr(runs$STATE, 5, 5)
runs <- runs[order(runs$Outs), ] 

# Create the matrix 
runs.out <- matrix(runs$RUNS_PR, 8, 3) # b/c it's sorted by outs, and there are 8 states, this puts the outs into cols
dimnames(runs.out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(runs.out)[[1]] <- c("000", "001", "010", "011", "100", "101", "110", "111")
runs.out


#########################################
## 5. Runner Advancement with a Single ##
#########################################

## a. select the plays where a single was hit 
d.single <- subset(data2011, EVENT_CD == 20)

## b. construct a table of frequencies of the vars STATE and NEW.STATE 
d.single$POS <- substr(d.single$STATE, 1, 3)
d.single$NEW.POS <- substr(d.single$NEW.STATE, 1, 3)

table(d.single$POS, d.single$NEW.POS)


## c. suppose there is a single runner on first base: using the above table, is it more likely for 
##    the lead runner to move to second, or to third base? 

# Lead runner is most likely to advance to second base 

## d. suppose instead there are runners on first and second. where do they move? 

# they mostly stay on 1st and second (i.e. they throw out the lead runner), or they load the bases 

## compute the probability a run is scored on the play: 
d.single$RUNS_F <- d.single$RUNS.SCORED > 0 
runs <- ddply(d.single, 'POS', summarize, 
              RUNS_PR = round(mean(RUNS_F), 3))
runs # probability a run is scored, is 0.639 

