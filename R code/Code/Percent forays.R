library(polynom)
library(ggplot2)
library(dplyr)
library(plotrix)
library(Rmisc)

setwd('C:/Users/zintl/Google Drive/Zintle_Oxalis pes-caprae')
data=read.csv("prophar.csv", header=TRUE)
data$Hours<-as.factor(data$grp)

p<-ggplot(data, aes(x = co2, y = perc, group=Hours))
p<-p+geom_point(aes(color=Hours,shape=Hours), size=5)
p<-p+ scale_colour_manual(values = c("black", "grey40"))
p<-p+ scale_shape_manual(values = c(16,17))
p

p<-p+ stat_smooth(data=data,method = "lm", formula = y ~ poly(x,2),size = .5,se=FALSE, color="black")
p

p<-p + theme(legend.position = c(.15,.85))
p

###Make pretty

p<-p+labs(x=expression(Growth~CO["2"]~~(ppm)),y=expression("%"~trips~achieving~2000~calories))
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

