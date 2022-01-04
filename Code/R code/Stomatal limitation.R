library(ggplot2)
library(dplyr)
library(plotrix)


setwd("~/(1) Research/(a) C3-C4 Comparisons/Alloteropsis/VPD Responses")

data=read.csv("c4vpd.csv", header=TRUE)
#calculate A/gst
agsw=a/gsw
# add it to data frame "data"
data<-cbind(data,agsw)
summary(data)


#calculate means and se
summary <- data %>% group_by(rep) %>% summarise_each(funs(mean,std.error))

# plot means and Se
p<-ggplot(summary, aes(x = vpdl_mean, y = gsw_mean)) + geom_point(size=5,color="red") 
p<- p+ geom_errorbar(aes(ymin=gsw_mean-gsw_std.error, ymax=gsw_mean+gsw_std.error), width=.1) 
p  

# Add actual points
r<-p+ geom_point(data = data, aes(x = vpdl, y = gsw, group=rep),shape=1,size=3)

#Add a smoothed line polynomial
r<-r+ stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1,color="red")
r
#######Adde axes titles
r<- r + labs(x=expression(VPD[leaf]~~(kPa)),
             y=expression(g[SW]~~(mu~mol)))#~m^-2~s^-1)))
r<- r+ ylim(0,0.2)
r

#######Remove grids
r<- r + guides(fill=FALSE)
r<-r+theme(legend.position="none")
#########Axis text size and direction
r<- r + theme(text = element_text(size=18,face="bold"),axis.text.x = element_text(angle=0, vjust=1))
################Axis numbers
black.bold.12.text <- element_text(face = "bold", color = "black", size = 14)
r<-r+ theme(axis.text.x = black.bold.12.text)
r<-r+ theme(axis.text.y = black.bold.12.text)
##########Remove grid lines and set background white
r<- r +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
##########Black lines around pannels
r<- r+ theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
##########Strip at top of fig
r<-r + theme(strip.background=element_rect(fill="grey 80",colour="black"))
r
