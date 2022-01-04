library(polynom)
library(ggplot2)
library(dplyr)


setwd("C:/Users/Bobr/Google Drive/(2.2) Geophytes at subatmospheric CO2")
data=read.csv("Oxalis biomass.csv", header=TRUE)
summary(data)
attach(data)

#data$comp<-as.numeric(data$comp)
data
plot(biomass~co2)

##########Tell it to be a box plot of death vs time
p<- ggplot(data, aes(co2,biomass,group=co2))
p<-p+geom_boxplot()
p
p<- p + facet_wrap(~comp,scales="free")
p

