data<-read.delim(file = "clipboard", sep = "\t")
head(data)

library(ggplot2)
library(dplyr)
library(plotrix)
library(gridExtra)

data$CO2 = as.factor(data$CO2)
data$Nutrients = as.factor(data$Nutrients)
data$response = as.numeric(sub("," , ".", data$response))

# split the data into photosynthesis and stomatal limitation
pes_caprae = subset(data, Species == 'O.pes-caprae')
punctata = subset(data, Species == 'O.punctata')

# calculate means and se for pes-caprae 
summary_pes_caprae = pes_caprae %>%
  group_by(CO2, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(response),
    sd=sd(response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# calculate means and se for punctata 
summary_punctata = punctata %>%
  group_by(CO2, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(response),
    sd=sd(response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# plot bar graph for  pes-caprae
p = ggplot(summary_pes_caprae) +
    geom_bar(color = 'black', fill = 'black', aes(x=CO2, y=mean, fill = Nutrients), stat="identity")

p = p+labs(x=expression(Growth~CO["2"]~~(ppm)),y=expression(A~~(mu~mol~m^-2~s^-1)))

# remove legend
p = p + guides(fill=FALSE)
# axis text size and direction
p = p + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
p = p +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
p = p+ theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
p = p + theme(strip.background=element_rect(fill="grey 80",colour="black"))
p

# plot bar graph for punctata
p1 = ggplot(summary_punctata) +
  geom_bar(aes(x=CO2, y=mean, fill = Nutrients), stat="identity", position = position_dodge()) +
  scale_fill_manual(values = c('black', 'grey'))

#p = ggplot(summary_photo) +
#    geom_bar(aes(x=CO2, y=mean, fill = Nutrients), stat="identity", position = position_dodge()) +
#    geom_errorbar(aes(x=CO2, ymin=mean-se, ymax=mean+se), width=0.1, colour="black", alpha=0.9, size=0.5, position = 'dodge')

p1 = p1 + labs(x=expression(Growth~CO["2"]~~(ppm)),y=expression( ))

# place the legend inside the plot
p1 = p1 + theme(
  legend.position = c(.5, .95), 
  legend.justification = c('right', 'top'), 
  legend.box.just = 'right', 
  legend.margin = margin(6, 6, 6, 6)
)


#########Axis text size and direction
p1 = p1 + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
##########Remove grid lines and set background white
p1 = p1 +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
##########Black lines around pannels
p1 = p1 + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
##########Strip at top of fig
p1 = p1 + theme(strip.background=element_rect(fill="grey 80",colour="black"))
p1

# combine both plots into one plot with 2 rows
grid.arrange(p, p1, ncol = 2)
