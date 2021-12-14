############libraries

library(lsmeans)
library(multcompView)
library(ggplot2)

###########Read clipboard

data=read.table(file="clipboard",header=TRUE,sep = "\t",na.strings=c("na","NA"))

summary(data)



############Read from file

setwd("~/(1) Research/Marion/2017")

data=read.csv("tempabove.csv", header=TRUE)

summary(data)

attach(data)

#################Do linear model and Anova

mod2<-lm(Roots~Nutrients,data=data)

anova (mod2, test="F")



###########Conduct post-hoc tests

leastsquare = lsmeans(mod2, pairwise~Nutrients, adjust="tukey")

library(lsmeans)
library(multcompView)

summary (leastsquare)

cld(leastsquare,alpha=.05,Letters=letters)



############Plot boxplot

p<-ggplot(data, aes(x=Nutrients, y=Roots))

p<-p+ geom_boxplot(outlier.size=0,notch=TRUE)

p

############Axes titles

p<- p + labs(x=expression(Roots~Nutrients[]),y=expression(Roots[]))

p<- p + guides(fill=FALSE)

p<-p+theme(legend.position="none")



#########Axis text size and direction

p<- p + theme(text = element_text(size=18,face="bold"),axis.text.x = element_text(angle=0, vjust=1))



################Axis numbers

black.bold.12.text <- element_text(face = "bold", color = "black", size = 10)

p<-p+ theme(axis.text.x = black.bold.12.text)

p<-p+ theme(axis.text.y = black.bold.12.text)



##########Remove grid lines and set background white

p<- p +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))



##########Black lines around pannels

p<- p+ theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))



##########Strip at top of fig

p<-p + theme(strip.background=element_rect(fill="grey 80",colour="black"))

p

#######Add mean and 95% confidence intervals

p<-p+stat_summary(fun.y=mean,geom="point",color="red",shape=4, size=3)

p<-p+stat_summary(fun.data = mean_cl_boot, conf.int = .95,geom="errorbar",color="red",width=0.4)           

p