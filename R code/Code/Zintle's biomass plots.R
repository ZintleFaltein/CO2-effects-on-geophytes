library(polynom)
library(ggplot2)
library(dplyr)
library(polynom)

setwd("C:/Users/Bobr/Google Drive/(2.2) Geophytes at subatmospheric CO2")
data=read.csv("Oxalis biomass.csv", header=TRUE)
summary(data)
attach(data)

plot(biomass~co2)
install.packages("tidyverse")
library("tidyverse")
df<-filter(data, comp=="Bulbs")
##########Tell it to be a box plot of death vs time
p<- ggplot(data, aes(co2,biomass))
p<-p+geom_smooth(data=filter(data, comp=="Bulbs"),aes(co2,biomass),method = "lm", se = TRUE, 
                 formula = y ~ poly(x, 2, raw = TRUE),size=1,color="black")
p<-p+geom_boxplot(aes(group=co2),fill="transparent")
p<- p + facet_wrap(~comp,scales="free")
p
###########Label axes
p<-p+labs(x=expression(Growth~CO["2"]~~(ppm)),y=expression(Biomass~(g)))
p

#########Removes legend
p<- p + guides(fill=FALSE)
#########Axis text size and direction
p<- p + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
##########Remove grid lines and set background white
p<- p +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
##########Black lines around pannels
p<- p+ theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
##########Strip at top of fig
p<-p + theme(strip.background=element_rect(fill="grey 80",colour="black"))
p
