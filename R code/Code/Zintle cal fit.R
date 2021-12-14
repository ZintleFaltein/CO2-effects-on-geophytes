library(polynom)
library(ggplot2)
library(dplyr)
library(plotrix)
library(Rmisc)

setwd("C:/Users/zintl/Google Drive/Zintle_Oxalis pes-caprae")
data=read.csv("hrscal.csv", header=TRUE)
data

avedata <- summarySE(data, measurevar="hours", groupvars="co2")
plot<-ggplot(data=avedata, aes(x = co2, y = hours)) + geom_point(size=3,color="black") 
plot

p<-plot+ stat_smooth(data=data,method = "lm", formula = y ~ poly(x,2), size = 1,color="black",se=TRUE)
p


###Make pretty

p<-p+labs(x=expression(Growth~CO["2"]~~(ppm)),y=expression(Time~to~collect~2000~cal~~(hours)))
p
p<- p + guides(fill=FALSE)
p

#########Axis text size and direction
p<- p + theme(text = element_text(size=18,face="bold"),axis.text.x = element_text(angle=0, vjust=1))
################Axis numbers
black.bold.12.text <- element_text(face = "bold", color = "black", size = 14)
p<-p+ theme(axis.text.x = black.bold.12.text)
p<-p+ theme(axis.text.y = black.bold.12.text)
##########Remove grid lines and set background white
p<- p +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
p

##########Black lines around pannels
p<- p+ theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
##########Strip at top of fig
p<-p + theme(strip.background=element_rect(fill="grey 80",colour="black"))
p

