data<-read.delim(file = "clipboard", sep = "\t")
summary(data)

library(ggplot2)
library(boot)
p<- ggplot(data, aes(co2,value,group=rep))
g_box0 <- p + geom_boxplot(fill = "transparent", colour = "black",outlier.shape = NA)
p<- g_box0 + facet_grid(~co2, scales = "free_x")
p
p<-p+labs(y=expression(Growth~CO["2"]~~(ppm)),x=expression( ))

#########Removes legend
 p<- p + guides(fill=FALSE)
p
 #########Axis text size and direction
 p<- p + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
 p
 ##########Remove grid lines and set background white
 p<- p +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
 p
 ##########Black lines around pannels
 p<- p+ theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
 p
  ##########Strip at top of fig
p<-p + theme(strip.background=element_rect(fill="grey 80",colour="black"))
 p