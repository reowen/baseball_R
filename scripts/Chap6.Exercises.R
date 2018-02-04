
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


###################################################
## 2. Comparing Pitch Locations for Two Pitchers ##
###################################################

## Compare Sanchez and Verlander's ability to maintain fastball speed throughout a game 
cols <- c('season', 'gamedate', 'pitches', 'speed') 
sanchez.ff <- sanchez[sanchez$pitch_type %in% c('FF', 'FT'), cols]
verlander.ff <- verlander[verlander$pitch_type %in% c('FF', 'FT'), cols]

sanchez.ff$pitcher <- "Sanchez" 
verlander.ff$pitcher <- "Verlander"

fastballs <- rbind(sanchez.ff, verlander.ff)
avg.fastball <- ddply(fastballs, c('pitcher', 'pitches'), summarize, 
                      avg.speed = round(mean(speed), 2))

library(ggplot2)
fb <- ggplot(data = avg.fastball, aes(x = pitches, y = avg.speed, color = pitcher)) + 
  geom_line() + 
  xlab("Number of Pitches") + 
  ylab("Average Fastball Speed") + 
  scale_x_continuous(breaks=seq(0, 135, 15))
fb


###################################################################
## 3. Graphical View of the Speeds of Justin Verlander's Pitches ##
###################################################################

## (A) Use the cut() function to group the verlander$pitches variable into groups of 10 pitches 
max.break <- ceiling(max(verlander$pitches)/10)*10 # rounds max pitch count up to nearest 10
verlander$pitch.bin <- with(verlander, cut(pitches, breaks=seq(0,max.break,10)))

## (B) Use lattice to construct a box plot of Verlander's four-seam fastball speed
F4verl <- subset(verlander, pitch_type == "FF") 
bwplot(speed~pitch.bin, data=F4verl)




