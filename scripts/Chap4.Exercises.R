
#####################################################################################
## 1. Relationship Between Winning Percentage, and Run Differential Across Decades ##
#####################################################################################

# A. Fit a predictive model for various decades 
teams <- read.csv("lahman/Teams.csv") 

genData <- function(yearMin, yearMax){
  cols <- c("teamID", "yearID", "lgID", "G", "W", "L", "R", "RA") 
  df <- subset(teams, yearID >= yearMin & yearID <= yearMax)[, cols] 
  
  # Run differential 
  df$RD <- with(df, R - RA)
  # Win percentage 
  df$Wpct <- with(df, W / (W + L))
  # Fit the model 
  linfit <- lm(Wpct ~ RD, data=df) 
  # Predicted values 
  df$linWpct <- predict(linfit) # cache the predicted values 
  df$linResiduals <- residuals(linfit) 
  
  # Pythagorean Values 
  df$pytWpct <- with(df, R ^ 2 / (R ^ 2 + RA ^ 2))
  df$pytResiduals <- df$Wpct - df$pytWpct
  
  return(list(df, linfit))
} 

linfit.1961.1970 <- genData(1961, 1970)[[2]]
linfit.1971.1980 <- genData(1971, 1980)[[2]] 
linfit.1981.1990 <- genData(1981, 1990)[[2]] 
linfit.1991.2000 <- genData(1991, 2000)[[2]] 

# B. Compare predicted winning percentages across decades, for team with RD of 10 
pred.1961 <- linfit.1961.1970$coefficients['RD'][[1]] * 10 * 100 
pred.1971 <- linfit.1971.1980$coefficients['RD'][[1]] * 10 * 100  
pred.1981 <- linfit.1981.1990$coefficients['RD'][[1]] * 10 * 100  
pred.1991 <- linfit.1991.2000$coefficients['RD'][[1]] * 10 * 100  

years <- c("1960's", "1970's", "1980's", "1990's") 
vals <- c(pred.1961, pred.1971, pred.1981, pred.1991) 
vals_fm <- c(paste(round(pred.1961, digits = 2), '%', sep=''), 
             paste(round(pred.1971, digits = 2), '%', sep=''), 
             paste(round(pred.1981, digits = 2), '%', sep=''), 
             paste(round(pred.1991, digits = 2), '%', sep=''))

xx <- barplot(vals, names.arg=years, ylim=c(0,0.85)) 
text(x = xx, y = vals, label=vals_fm, pos=3, cex=0.7) 


########################################################################### 
## 2. Pythagorean Residuals for Poor and Great Teams in the 19th Century ## 
########################################################################### 

teams.1800s <- genData(1800, 1899)[[1]] 

plot(teams.1800s$RD, teams.1800s$pytResiduals,
     xlab="run differential",
     ylab="residual") 
abline(h=0, lty=3) 
with(teams.1800s, lines(lowess(RD, pytResiduals, f=0.3))) 
#identify(teams.1800s$RD, teams.1800s$pytResiduals, labels=teams.1800s$teamID) 


################################################# 
## 3. Exploring the Manager Effect in Baseball ## 
################################################# 






