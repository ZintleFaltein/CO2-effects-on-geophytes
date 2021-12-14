data=read.table(file="clipboard",header=TRUE,sep =
                  "\t",na.strings=c("na","NA"))
data<-read.delim(file = "clipboard", sep = "\t")
summary(data)

##########ANOVA
a1 <- aov(Leaf.biomass ~ CO2, data=data) 
a1= aov(Leaf.biomass ~ CO2, data=data)
summary(a1)

a2 <- aov(total.biomass ~ Nutrients, data=data) 
a2= aov(total.biomass ~ Nutrients, data=data)
summary(a2)
###########Interaction
a3 <- aov(total.biomass ~ CO2*Nutrients, data=data) 
a3= aov(total.biomass ~ CO2*Nutrients, data=data)
summary(a3)

##########By each CO2

data1<-subset(data, Nutrients=="High")
b1<- aov(Leaf.biomass ~ CO2, data=data)
b1= aov(Leaf.biomass ~ CO2, data=data)
summary(b1)
pairwise.t.test(data$Bulb.weight, data$CO2, p.adj = "none")

data2<-subset(data, Nutrients=="Low")
b2<- aov(roots ~ CO2, data=data2)
b2= aov(roots~ CO2, data=data2)
summary(b2)
pairwise.t.test(data2$roots, data2$CO2, p.adj = "none")


data3<-subset(data, shade=="60% shade")
b3<- aov(root.length ~ co2, data=data3)
b3= aov(root.length ~ co2, data=data3)
summary(b3)
pairwise.t.test(data3$root.length, data3$co2, p.adj = "none")


##########By each nutrient treatment

data4<-subset(data, CO2=="400")
b4<- aov(roots ~ Nutrients, data=data4) 
b4= aov(roots ~ Nutrients, data=data4)
summary(b4)
pairwise.t.test(data4$roots, data4$Nutrients, p.adj = "none")


data5<-subset(data, CO2=="280")
b5<- aov(roots ~ Nutrients, data=data5) 
b5= aov(roots ~ Nutrients, data=data5)
summary(b5)
pairwise.t.test(data5$roots, data5$Nutrients, p.adj = "none")


data6<-subset(data, CO2=="180")
b6<- aov(roots ~ Nutrients, data=data6) 
b6= aov(roots ~ Nutrients, data=data6)
summary(b6)
pairwise.t.test(data6$roots, data6$Nutrients, p.adj = "none")

> results=aov(Bulb.weight~CO2, data=data)
> TukeyHSD(results, conf.level = 0.95)
