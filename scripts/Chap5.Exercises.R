
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

