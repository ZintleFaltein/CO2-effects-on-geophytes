# load data and view head
data<-read.delim(file = "clipboard", sep = "\t")
head(data)

# load libraries
library(ggplot2)
library(gridExtra)

# convert nutrient column to a factor
data$Nutrients <- as.factor(data$Nutrients)
# convert response column to numeric
data$response <- as.numeric(sub("," , ".", data$response))

# subset data to get jmax and vcmax
jmax = subset(data, comp == 'Jmax')
vcmax = subset(data, comp == 'Vcmax')

# plot boxplots for jmax
p1 = ggplot(jmax, aes(Nutrients,response))
p1 = p1+geom_boxplot(aes(group=Nutrients),fill="transparent")
p1 = p1+labs(x=expression(Nutrient~concentration~~('%')),y=expression(J['max']~~(mu~mol~m^-2~s^-1)))

# remove legend
p1 = p1 + guides(fill=FALSE)
# axis text size and direction
p1 = p1 + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
p1 = p1 +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# black lines around pannels
p1 = p1 + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# strip at top of fig
p1 = p1 + theme(strip.background=element_rect(fill="grey 80",colour="black"))

# plot boxplots for vcmax
p2 = ggplot(vcmax, aes(Nutrients,response))
p2 = p2+geom_boxplot(aes(group=Nutrients),fill="transparent")
p2 = p2+labs(x=expression(Nutrient~concentration~~('%')),y=expression(V['cmax']~~(mu~mol~m^-2~s^-1)))

# remove legend
p2 = p2 + guides(fill=FALSE)
# axis text size and direction
p2 = p2 + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
p2 = p2 +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# black lines around pannels
p2 = p2 + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# strip at top of fig
p2 = p2 + theme(strip.background=element_rect(fill="grey 80",colour="black"))
p2

# combine plots in one plot that has 2 columns
grid.arrange(p1, p2, ncol = 2)
