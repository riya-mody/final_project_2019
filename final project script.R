rm(list=ls())

setwd("~/Desktop/comp sci project datasets")

happiness<-read.csv("2017.csv")

plot(happiness$Happiness.Score, happiness$Family) #Correlation btwn happiness and family

happyfamily.lm<-lm(Family~Happiness.Score,data=happiness) #Linear model
summary(happyfamily.lm) #Shows stats
abline(happyfamily.lm,col="red") #Inserts regression line for correlation 

268 #Page for correlation 
