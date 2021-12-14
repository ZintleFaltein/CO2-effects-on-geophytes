setwd("C:/Users/zintl/OneDrive - campus.ru.ac.za/M.Sc Project/Special issue")
rm(list=ls())
data1 <- read.csv("Chamber1.csv")
summary(data1)
### If your data sets are as big as mine were you might have to merge multiple excel sheets in R
### Merge 3 datasets (becuase they were too big for excel)
### total <- rbind(data1, data2, data3)
### summary(total)

### Plot CO2 averages for each chamber
library(ggplot2)
data1$chamber <- as.factor(data1$chamber)
p <- ggplot(data1, aes(x=chamber, y=CO2.avg)) +
  geom_boxplot()
p

### Calculate the CO2 chamber efficiency by chamber
# e.g. chamber 1
ch1 <- subset(data1, chamber == "1")
mean(ch1$CO2.avg)
sd(ch1$CO2.avg) ### In my case the sd was 25.33, which led to the 374.67 and 425.33 you see in the line below.

### This is a 400ppm chamber. Change the numbers in the lines of code below based on what your CO2 should be.
### This calculates how many of the recorded points were within the sd:
nosd <- sum(ch1$CO2.avg>137.9 & ch1$CO2.avg<222.1)
nosd

### This calculates how many of the recorded points were within 50ppm of the CO2 you were trying to get:
no50 <- sum(ch1$CO2.avg>130 & ch1$CO2.avg<230)
no50

### This calculates how many of the recorded points were within 100ppm of the CO2 you were trying to get:
no100 <- sum(ch1$CO2.avg>80 & ch1$CO2.avg<280)
no100

### Do this for no sd, no50 and no100 to get % of recorded points within each range
percent = no100/871*100
percent
