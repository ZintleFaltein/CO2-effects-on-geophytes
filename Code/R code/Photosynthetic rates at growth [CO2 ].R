co16<- subset(data, Rep=="16")
co17<- subset(data, Rep=="17")
co18<- subset(data, Rep=="18")
co19<- subset(data, Rep=="19")


f16<-fitaci(co16)
f17<-fitaci(co17)
f18<-fitaci(co18)
f19<-fitaci(co19)


f16$Photosyn(Ca=400)
f17$Photosyn(Ca=400)
f18$Photosyn(Ca=400)
f19$Photosyn(Ca=400)

f16$Photosyn(Ci=400)
f17$Photosyn(Ci=400)
f18$Photosyn(Ci=400)
f19$Photosyn(Ci=400)
