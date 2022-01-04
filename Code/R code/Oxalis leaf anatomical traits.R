# This script plots the changes in leaf anatomical traits of Oxalis punctata

data<-read.delim(file = "clipboard", sep = "\t")

library(ggplot2)
library(ggpmisc)
library(dplyr)
library(gridExtra)

data$CO2 = as.factor(data$CO2)
data$Nutrients = as.factor(data$Nutrients)
data$Response = as.numeric(sub("," , ".", data$Response))
data$Trait = as.factor(data$Trait)

str(data)
head(data)

uet <- ggplot(data = data %>%
                 filter(Trait == 'Upper epidermis'), aes(CO2,Response))
uet <- uet + geom_boxplot(aes(group=CO2),fill="transparent")
uet <- uet + facet_wrap(~Nutrients)

uet <- uet + labs(x=expression(' '),
               y=expression(UET~(mu~m)))
#uet

uet = uet + guides(fill=FALSE)
# axis text size and direction
uet = uet + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
uet= uet + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
uet= uet + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
uet= uet + theme(strip.background=element_rect(fill="grey 80",colour="black"))

let <- ggplot(data=filter(data, Trait == 'Lower epidermis'), aes(CO2,Response))
let <- let +geom_boxplot(aes(group=CO2),fill="transparent")
let <- let + facet_wrap(~Nutrients)

let = let + labs(x=expression(' '),
                 y=expression(LET~(mu~m)))
#let

let = let + guides(fill=FALSE)
# axis text size and direction
let = let + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
let= let + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
let= let + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
let= let + theme(strip.background=element_rect(fill="grey 80",colour="black"))

lt <- ggplot(data=filter(data, Trait == 'Leaf thickness'), aes(CO2,Response))
lt <- lt +geom_boxplot(aes(group=CO2),fill="transparent")
lt <- lt + facet_wrap(~Nutrients)

lt <- lt + labs(x=expression(Growth~CO["2"]~~(ppm)),
                y=expression(LT~(mu~m)))

lt = lt + guides(fill=FALSE)
# axis text size and direction
lt = lt + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
lt= lt + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
lt= lt + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
lt= lt + theme(strip.background=element_rect(fill="grey 80",colour="black"))

grid.arrange(uet, let, lt, nrow = 3)
