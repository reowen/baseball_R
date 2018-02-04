
library(plyr)
library(lattice)
# Import all of the objects from the Chapter 5 notes 
source('../scripts/Chap6.advanced.graphics.R')

###############################################################
## 1. Location of Pitches for Left- and Right-Handed batters ##
###############################################################

## Use a density plot to plot the horizontal location of Verlander's pitches, by batter handedness 

# define the boundaries of the strike zone
topKzone <- 3.5
botKzone <- 1.6
inKzone <- -.95
outKzone <- 0.95

densityplot(~ px, data = verlander, groups = batter_hand, 
            plot.points = FALSE, 
            auto.key = TRUE, 
            xlab="Horizonal Location of Pitches", 
            ylab="Density of Pitch Location", 
            panel = function(x, ...){
              panel.densityplot(x, ...) 
              panel.abline(v=inKzone) 
              panel.abline(v=outKzone)
            })
