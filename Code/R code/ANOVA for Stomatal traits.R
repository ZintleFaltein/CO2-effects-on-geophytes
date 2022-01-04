# This script determines if there is an effect of CO2
# and nutrients on stomatal traits.
# It also look for significant differences within treatments

library(dplyr)
library(readr)

stomatal_measurements <- read_delim("Data/Oxalis combined stomatal measurements.csv", ";", escape_double = FALSE, trim_ws = TRUE)
head(stomatal_measurements)

stomatal_measurements$CO2 <- as.factor(stomatal_measurements$CO2)
stomatal_measurements$Nutrients <- as.factor(stomatal_measurements$Nutrients)
stomatal_measurements$Response <- as.numeric(sub("," , ".", stomatal_measurements$Response))
stomatal_measurements$Trait <- as.factor(stomatal_measurements$Trait)

# Stomatal density
density.lm<-lm(Response ~ CO2*Nutrients, data = filter(stomatal_measurements, Trait == 'Stomatal density'))
summary(density.lm)
density.lm<-lm(Response ~ CO2, data = filter(stomatal_measurements, Trait == 'Stomatal density'))
summary(density.lm)
density.lm<-lm(Response ~ Nutrients, data = filter(stomatal_measurements, Trait == 'Stomatal density'))
summary(density.lm)

# Do a Tukey post-hoc test to see differences within treatments
low_nutrient <- filter(stomatal_measurements, Nutrients == '50%Long Ashton')
low.aov<-aov(Response ~ CO2, data = filter(low_nutrient, Trait == 'Stomatal density'))
TukeyHSD(low.aov, conf.level = 0.95)

high_nutrient <- filter(stomatal_measurements, Nutrients == '100%Long Ashton')
high.aov<-aov(Response ~ CO2, data = filter(high_nutrient, Trait == 'Stomatal density'))
TukeyHSD(high.aov, conf.level = 0.95)

# Stomatal size
size.lm<-lm(Response ~ CO2*Nutrients, data = filter(stomatal_measurements, Trait == 'Stomatal size'))
summary(size.lm)
size.lm<-lm(Response ~ CO2, data = filter(stomatal_measurements, Trait == 'Stomatal size'))
summary(size.lm)
size.lm<-lm(Response ~ Nutrients, data = filter(stomatal_measurements, Trait == 'Stomatal size'))
summary(size.lm)

# Do a Tukey post-hoc test to see differences within treatments
low_nutrient <- filter(stomatal_measurements, Nutrients == '50%Long Ashton')
low.aov<-aov(Response ~ CO2, data = filter(low_nutrient, Trait == 'Stomatal size'))
TukeyHSD(low.aov, conf.level = 0.95)

high_nutrient <- filter(stomatal_measurements, Nutrients == '100%Long Ashton')
high.aov<-aov(Response ~ CO2, data = filter(high_nutrient, Trait == 'Stomatal size'))
TukeyHSD(high.aov, conf.level = 0.95)

# Stomatal index
index.lm<-lm(Response ~ CO2*Nutrients, data = filter(stomatal_measurements, Trait == 'Stomatal index'))
summary(index.lm)
index.lm<-lm(Response ~ CO2, data = filter(stomatal_measurements, Trait == 'Stomatal index'))
summary(index.lm)
index.lm<-lm(Response ~ Nutrients, data = filter(stomatal_measurements, Trait == 'Stomatal index'))
summary(index.lm)

# Do a Tukey post-hoc test to see differences within treatments
low_nutrient <- filter(stomatal_measurements, Nutrients == '50%Long Ashton')
low.aov<-aov(Response ~ CO2, data = filter(low_nutrient, Trait == 'Stomatal index'))
TukeyHSD(low.aov, conf.level = 0.95)

high_nutrient <- filter(stomatal_measurements, Nutrients == '100%Long Ashton')
high.aov<-aov(Response ~ CO2, data = filter(high_nutrient, Trait == 'Stomatal index'))
TukeyHSD(high.aov, conf.level = 0.95)
