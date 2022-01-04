data<-read.delim(file = "clipboard", sep = "\t")

head(data)

#Load libraries
library(ggplot2)

#Change Rep and Nutrients data types to factors
data$CO2 <- as.factor(data$CO2)
data$Nutrients <- as.factor(data$Nutrients)
data$Species <- as.factor(data$Species)
#Change Photo to numeric
data$Photo <- as.numeric(sub("," , ".", data$Photo))

#Split the data into the respective CO2 concentrations
ppm_180 <- subset(data, Rep == "180")
ppm_280 <- subset(data, Rep == "280")
ppm_400 <- subset(data, Rep == "400")

#Plot line graph
p <-ggplot(data, aes(x = Ci, y = Photo)) + 
  geom_line(aes(linetype = Nutrients)) + 
  geom_point(aes(shape = Nutrients)) 
p <- p + facet_wrap(~Rep)  

#Set axis labels                  
p<-p+labs(x=expression(Ci~~(ppm)),y=expression(A~~(mu~mol~m^-2~s^-1)))

#Remove legend
p<- p + guides(fill=FALSE)
#Axis text size and direction
p<- p + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
#Remove grid lines and set background to white
p<- p +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
#Black lines around pannels
p<- p+ theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
#Strip at top of fig
p<-p + theme(strip.background=element_rect(fill="grey 80",colour="black"))
p

# to place the legend on the bottom right corner of the figure use the following code
p<-p + theme(
  legend.position = c(.95, .45),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)
