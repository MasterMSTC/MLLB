#############################################################
###########  WORKING Linear Regression on Advertising data
#############################################################

# List objects in this session
list=ls()

# remove all (memory clean)
rm(list=ls())

# Set Working directory
setwd("D:/MSTC_BD/R/Curso_2019_20/LINEAR_REGRESSION/DATA")

#Read the csv file into Advertising
Advertising=read.csv("Advertising.csv")
fix(Advertising)

# Explore scatter plots
plot(Advertising$TV,Advertising$Sales)
attach(Advertising)
plot(TV,Sales)

par(mfrow=c(2,2))
plot(TV,Sales)
plot(Radio,Sales)
plot(Newspaper,Sales)

# Explore scatter plots
pairs(~ TV + Radio + News.paper + Sales, Advertising) 

# More advanced ggplot (2)
# https://deanattali.com/2015/03/29/ggExtra-r-package/

#install.packages("ggExtra")

#library(ggplot2)
# create a ggplot2 scatterplot
#p <- ggplot(Advertising, aes(TV, Sales)) + geom_point() + theme_classic()
# add marginal histograms
#ggExtra::ggMarginal(p, type = "histogram")

# You can start trying a simple linear regression:
# Predicting Sales only from TV budget
# Predicting Sales only from Newspaper budget
# Predicting Sales only from Radio budget

#Simple Linear Regression: Sales ~ TV
lmTV.fit=lm(Sales~TV,data=Advertising)

# or using attach previouly
attach(Advertising)
lmTV.fit=lm(Sales~TV)

# Plotting the Regression Line
par(mfrow=c(1,1))
plot(TV,Sales)
abline(lmTV.fit,lwd=3,col=2)

summary(lmTV.fit)

######## NEWSPAPER ######################
#Simple Linear Regression: Sales ~ Newspaper
lmNewspaper.fit=lm(Sales~Newspaper)

# Plotting the Regression Line
par(mfrow=c(1,1))
plot(Newspaper,Sales)
abline(lmNewspaper.fit,lwd=3,col=2)

summary(lmNewspaper.fit)

#### Note ; no prediction ... predict the Mean
###            a Naive predictor to compare with

######### RADIO ####################
#Simple Linear Regression: Sales ~ Radio
lmRadio.fit=lm(Sales~Radio)

# Plotting the Regression Line
par(mfrow=c(1,1))
plot(Radio,Sales)
abline(lmRadio.fit,lwd=3,col=2)

summary(lmRadio.fit)

