setwd("C:/Users/zintl/OneDrive - campus.ru.ac.za/M.Sc Project")
data=read.csv("Data for mixed effects model-QSR.csv", header=TRUE)

library(lme4)
library(car)
plot((Biomass)~CO2)

bio<-lmer((Biomass)~CO2*(1|Chamber))

Anova(bio,test.statistic=c("F"), type=2)

r.squaredLR(bio)