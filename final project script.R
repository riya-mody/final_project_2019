rm(list=ls())

setwd("~/Desktop/comp sci project datasets")

data.facial<-read.csv("facial_expression.csv")
data.facial[data.facial=="FIT_FAILED"]<-NA
data.facial[data.facial=="FIND_FAILED"]<-NA
data.facial2<-na.omit(data.facial)

library(ggplot2)

pdf("bad.graph.pdf")
G<-ggplot(data.facial2, aes(x=Video.Time, y=Happy)) + geom_point() + 
  ggtitle("Happiness over Time") +
  xlab("Video Time") +
  ylab("Happiness Levels")
G
dev.off()