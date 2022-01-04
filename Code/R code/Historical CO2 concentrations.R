# This script plots the changes in [CO2] in the last 800 000 years.

library(dplyr)
library(ggplot2)

data <- read.csv("https://raw.githubusercontent.com/ZintleFaltein/CO2-effects-on-geophytes/master/R%20code/Data/carbon.csv?token=AOMLEIBDSBOBAMO6F2JTA53BYTHEQ", header=TRUE, sep = ';')

head(data)

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
conc <- ggplot(data, aes(x=age, y=co2)) +
  geom_line() + 
  xlab(NULL)

#Zoom in to get CO2 values between 123 & 195 kya
#This will create a second plot
zoomed_conc <- ggplot(data, aes(x=age, y=co2)) +
  geom_line() +
  scale_x_continuous(limits = c(123000, 195000))+
  scale_y_continuous(limits = c(150, 300)) +
  #theme(axis.text.x = element_text(angle = 45)) +
  labs(y=expression(CO["2"]~~(ppm)), x=NULL) +
  themed

#Make the final plot with the inset
conc <- conc + 
  annotation_custom(ggplotGrob(zoomed_conc), xmin = 500000, xmax = 800000, 
                       ymin = 280, ymax = 360) + 
  #add annotations (text and rectangular strip)
  #alpha=0.5 sets the intensity of the rectangle colour
  annotate("rect", xmin = 190000, xmax=200000, ymin=170, ymax=300, alpha=0.5) +
  annotate("text", x=190000, y=305, label="modern human emergence", size=2.5) +
  themed

conc

