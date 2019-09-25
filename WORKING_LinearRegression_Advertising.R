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
pairs(~ TV + Radio + Newspaper + Sales, Advertising) 

# More advanced ggplot (2)
# https://deanattali.com/2015/03/29/ggExtra-r-package/

#install.packages("ggExtra")

#library(ggplot2)
# create a ggplot2 scatterplot
#p <- ggplot(Advertising, aes(TV, Sales)) + geom_point() + theme_classic()
# add marginal histograms
#ggExtra::ggMarginal(p, type = "histogram")


# Multiple Linear Regression
# lm.fit=lm(Sales~TV+Radio+Newspaper,data=Advertising)

# ~.-X = all variables but X (as X is not a variable  . it means use ALL !)

lm.fit=lm(Sales~.-X,data=Advertising)
summary(lm.fit)


##### SOME FURTHER TOPICS to study ######
#########################################

#Go back to a Simple Linear Regression: Sales ~ TV
lm.fit=lm(Sales~TV)


# R^2 and correlation with predicted values Corr(Y,Y_hat)
summary(lm.fit)
Pred_Sales=predict(lm.fit,data.frame(TV),interval="prediction")
cor(Sales,Pred_Sales)^2

# As it is a simple prediction Check it is also Cor(X,Y) 
cor(Sales,TV)^2

# Other metrics
plot(abs(Sales-Pred_Sales[,1]),xlab="Sales",type='p',pch=12,col='blue')

MAE=sum(abs(Sales-Pred_Sales[,1]))/length(Sales)


# CONFIDENCE and PREDICTION intervals
# 95% confidence interval associated with each TV value
predict(lm.fit,data.frame(TV=c(10,100,200)),interval="confidence")

# prediction interval for predicted Sales
predict(lm.fit,data.frame(TV=c(10,100,200)),interval="prediction")



# INTERACTION TERMS
lm.fit=lm(Sales~TV*Radio,data=Advertising)
summary(lm.fit)

# NON-LINEARITIES
# Residual plots are a useful graphical tool for identifying non-linearity 
# Plotting residualts
plot(predict(lm.fit),residuals(lm.fit),xlab='Fitted values',ylab='Residuals',main='Residual Plot for Advertisement')


# OUTLIERS
# Plotting studentized residuals
plot(predict(lm.fit),rstudent(lm.fit),xlab='Fitted values',ylab='Studentized Residuals',main='Studentized Residual Plot for Advertisement')


# par(mfrow=c(2,2)) divides plot region in 2 x 2
par(mfrow=c(2,2)) 
plot(lm.fit) 

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

