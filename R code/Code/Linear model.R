plot(log(Biomass)~amax.g)

bio<-lmer(log(Biomass)~amax.g*(1|Species))

Anova(bio,test.statistic=c("F"), type=2)

r.squaredLR(bio)