data <- read.delim(file = "clipboard", sep = "\t")

data$co2 <- as.factor(data$co2)
data$Nutrients <- as.factor(data$Nutrients)

#Build the model to determine if there is a treatment effect
output <- glm(formula = biomass ~ co2 + Nutrients, data = data, family = poisson)

summary(output)

#Post hoc test for significant differences within treatments
bio.aov<-aov(biomass ~ co2, data = data)
TukeyHSD(bio.aov, conf.level = 0.95)
