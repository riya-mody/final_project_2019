rm(list=ls())
setwd("~/Desktop/comp sci project datasets")
data.facial<-read.csv("facial_expression.csv")
data.facial[data.facial=="FIT_FAILED"]<-NA
data.facial[data.facial=="FIND_FAILED"]<-NA
data.facial2<-na.omit(data.facial)