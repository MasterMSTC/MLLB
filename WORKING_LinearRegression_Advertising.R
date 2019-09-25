# List objects in this session
list=ls()

# remove all (memory clean)
rm(list=ls())

# Set Working directory
setwd("D:/MSTC_BD/R/Curso_2019_20/LINEAR_REGRESSION/DATA")

#Read the csv file into Advertising
Advertising=read.csv("Advertising.csv")
fix(Advertising)

