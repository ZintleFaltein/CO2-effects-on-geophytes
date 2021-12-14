data=read.table(file="clipboard",header=TRUE,sep =
                  "\t",na.strings=c("na","NA"))
data<-read.delim(file = "clipboard", sep = "\t")
head(data)

data$CO2<-as.factor(data$CO2)
data$biomass<-as.numeric(data$biomass)

bio.lm<-lm(biomass ~ CO2*Nutrients, data = punctata)
summary(bio.lm)

##Do a Tukey post-hoc test to see differences within treatments
bio.aov<-aov(biomass ~ CO2, data = punctata)
TukeyHSD(bio.aov, conf.level = 0.95)
