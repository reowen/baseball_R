
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

hofpitching <- read.csv('hofpitching.csv') 
hofpitching$BF.group <- with(hofpitching, cut(BF, c(0,10000,15000,20000,30000), 
                                              labels=c('Less than 10000', '(10000, 15000)', '(15000,20000)', 
                                                       'More than 20000')))

### A. Construct a frequency table of BF.group using the table function 
BF.table <- table(hofpitching$BF.group)
BF.table

### B. Construct a bargraph with said table 
freqs <- as.numeric(BF.table) 
cats <- dimnames(BF.table)
xx <- barplot(BF.table, ylim=c(0,30))
text(x = xx, y = freqs, label=freqs, pos=3)

### C. Construct piechart 
# xx <- pie(BF.table, labels=freqs)
# legend("topright", legend=xx, labels=cats) 
pie(BF.table)


####################################################
### 2. Hall of Fame Pitching Dataset (continued) ###
####################################################
hofordered <- hofpitching[order(hofpitching$WAR, decreasing=TRUE), ] 
pitchers <- as.character(hofordered[1:2, 'X'])
xpos <- as.numeric(hofordered[1:2, 'WAR'])

with(hofpitching, hist(WAR))
# with(hofpitching, identify(WAR, X, n=4))
text(x=xpos, y=c(3,5), labels=pitchers, cex=0.7)


####################################################
### 3. Hall of Fame Pitching Dataset (continued) ###
####################################################

hofpitching$WAR.season <- with(hofpitching, WAR / Yrs)

x <- par(plt = c(.25, .94, .145, .883))
stripchart(WAR.season ~ BF.group, data = hofpitching, 
           method="jitter", pch=1, las=2) 
x
boxplot(WAR.season ~ BF.group, data=hofpitching, las=2,
        horizontal=TRUE, xlab="WAR")


####################################################
### 4. Hall of Fame Pitching Dataset (continued) ###
#################################################### 

hofpitching$MidYear <- with(hofpitching, (From + To) / 2)
hofpitching.recent <- subset(hofpitching, MidYear >= 1960)

## A. Order by WAR.seasons
hofpitching.recent <- hofpitching.recent[order(hofpitching.recent$WAR.season), ]

## B. Construct a Dotplot
with(hofpitching.recent, dotchart(WAR.season, labels=X, xlab="WAR per Season"))


####################################################
### 5. Hall of Fame Pitching Dataset (continued) ###
#################################################### 

## Construct a scatterplot of WAR.season and MidYear, ID trends, and ID two pitchers in 1800's with low WAR 
with(hofpitching, plot(MidYear, WAR.season))
with(hofpitching, lines(lowess(MidYear, WAR.season, f=0.3))) 
with(hofpitching, identify(MidYear, WAR.season, X, n=4, pos=1, cex=0.7))


##################################################
### 6. Working with the Lahman Batting Dataset ###
################################################## 




