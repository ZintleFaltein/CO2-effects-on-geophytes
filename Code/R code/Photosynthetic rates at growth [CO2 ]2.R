data<-read.delim(file = "clipboard", sep = "\t")

library(plantecophys)

# Subset the data into the respective CO2 concentrations

#Get individual ACi curves for each plant
co1<- subset(data, Rep=="1")
co2<- subset(data, Rep=="2")
co3<- subset(data, Rep=="3")
co4<- subset(data, Rep=="4")
co5<- subset(data, Rep=="5")
co6<- subset(data, Rep=='6')
co7<- subset(data, Rep=="7")
co8<- subset(data, Rep=="8")
co9<- subset(data, Rep=="9")
co10<- subset(data, Rep=="10")

#Fit curves individually

f1<-fitaci(co1)
f2<-fitaci(co2)
f3<-fitaci(co3)
f4<-fitaci(co4)
f5<-fitaci(co5)
f6<-fitaci(co6)
f7<-fitaci(co7)
f8<-fitaci(co8)
f9<-fitaci(co9)
f10<-fitaci(co10)

#Get photosynthetic rate at the growth [CO2]
#If growth [CO2]=180 set Ca=180

f1$Photosyn(Ca=400)
f2$Photosyn(Ca=400)
f3$Photosyn(Ca=400)
f4$Photosyn(Ca=280)
f5$Photosyn(Ca=280)
f6$Photosyn(Ca=280)
f7$Photosyn(Ca=180)
f8$Photosyn(Ca=180)
f9$Photosyn(Ca=180)
f10$Photosyn(Ca=180)

#Get photosynthetic rate at intercellular [CO2]
f1$Photosyn(Ci=400)
f2$Photosyn(Ci=400)
f3$Photosyn(Ci=400)
f4$Photosyn(Ci=280)
f5$Photosyn(Ci=280)
f6$Photosyn(Ci=280)
f7$Photosyn(Ci=180)
f8$Photosyn(Ci=180)
f9$Photosyn(Ci=180)
f10$Photosyn(Ci=180)

#Get Vcmax, Jmax and Rd
coef(f1)
coef(f2)
coef(f3)
coef(f4)
coef(f5)
coef(f6)
coef(f7)
coef(f8)
coef(f9)
coef(f10)