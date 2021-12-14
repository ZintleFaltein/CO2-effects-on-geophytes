#Set working directory and import the data
#setwd("C:/Users/zintl/OneDrive - campus.ru.ac.za/M.Sc Project/R code")
data=read.csv("Data/carbon.csv", header=TRUE, sep = ';')

head(data)

library(dplyr)
library(ggplot2)

# force R to plot actual values and not scientific notation
options(scipen=10000)

themed <- theme(panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                panel.background=element_rect(fill="white"),
                axis.line=element_line("black"),
                panel.border=element_rect(fill=NA,colour="black"),
                strip.background=element_rect(fill="grey 80",colour="black"),
                strip.text.x = element_text(size = 7),
                legend.position = 'right')

#Produce the plot
p <- ggplot(data, aes(x=age, y=co2)) +
  geom_line() + 
  xlab("")

#Zoom in to get CO2 values between 123 & 195 kya
#This will create a second plot
p2 <- ggplot(data, aes(x=age, y=co2)) +
  geom_line() +
  scale_x_continuous(limits = c(123000, 195000))+
  scale_y_continuous(limits = c(150, 300)) +
  #theme(axis.text.x = element_text(angle = 45)) +
  labs(y=expression(CO["2"]~~(ppm)), x=NULL) +
  themed

#Make the final plot with the inset
p<-p + annotation_custom(ggplotGrob(p2), xmin = 500000, xmax = 800000, 
                       ymin = 280, ymax = 360)

#Add annotations (text and rectangular strip)
#alpha=0.5 sets the intensity of the rectangle colour
p<-p + annotate("rect", xmin = 190000, xmax=200000, ymin=170, ymax=300, alpha=0.5) +
  annotate("text", x=190000, y=305, label="modern human emergence", size=2.5)

#Change the axis labels
p<-p +labs(x=expression(Age~('Years before present')), y=expression(CO["2"]~~(ppm)))
#Remove legend
p<- p + guides(fill=FALSE)
#Axis text size and direction
p<- p + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
##Remove grid lines and set background to white
p<- p +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
#Black lines around pannels
p<- p+ theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
#Strip at top of fig
p<-p + theme(strip.background=element_rect(fill="grey 80",colour="black"))
p