library(polynom)
library(ggplot2)

setwd("C:/Users/Bobr/Google Drive/(2.2) Geophytes at subatmospheric CO2")
data=read.csv("Oxalis biomass.csv", header=TRUE)
summary(data)



df <- data.frame("x"= data$cotwo, "y"=data$bulbs)
df<-na.omit(df)
df

my.formula <- y ~ poly(x, 2, raw = TRUE)
p <- ggplot(df, aes(x, y)) 
p <- p + geom_point(alpha=8/10, shape=21, fill="blue", colour="black", size=5)
p <- p + geom_smooth(method = "lm", se = TRUE, 
                     formula = my.formula, 
                     colour = "red",size=1)
p
m <- lm(my.formula, df)
my.eq <- as.character(signif(as.polynomial(coef(m)), 2))
label.text <- paste(gsub("x", "~italic(x)", my.eq, fixed = TRUE),
                    paste("italic(R)^2",  
                          format(summary(m)$r.squared, digits = 2), 
                          sep = "~`=`~"),sep = "~~~~")

p<-p + annotate(geom = "text", x = 200, y = 2.4, label = label.text, 
             family = "serif", hjust = 0, parse = TRUE, size = 5)
p

###Make pretty

p<-p+labs(x=expression(Growth~CO["2"]~~(ppm)),y=expression(Bulb~mass~~(g)))
p
p<- p + guides(fill=FALSE)
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


###Get equations for upper and loer confidence intervals
m <- lm(y ~ poly(x, 2, raw = TRUE), data = df)
mp <- predict(m, interval = 'conf')
df2 <- cbind(df, mp)
df2

mean<-lm(y~poly(x,2,raw=TRUE),data=df2)
mean

uci<-lm(upr ~ poly(x, 2, raw = TRUE), data = df2)
uci

lci<-lm(lwr ~ poly(x, 2, raw = TRUE), data = df2)
lci

m







