# This script calculates the mean, SD, SE and CI for stomatal size,
# stomatal density and index then plots bar graphs on the same set of axes

library(ggplot2)
library(dplyr)
library(plotrix)
library(gridExtra)
library(tidyverse)
library(readr)

# load data from github
stomatal_measurements <- read_delim("https://raw.githubusercontent.com/ZintleFaltein/CO2-effects-on-geophytes/master/R%20code/Data/Oxalis%20combined%20stomatal%20measurements.csv?token=AOMLEIAJIVDTF3CRVXZQRUDBYTGZY", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# create a custom theme for plots
themed <- theme(panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               panel.background=element_rect(fill="white"),
               axis.line=element_line("black"),
               panel.border=element_rect(fill=NA,colour="black"),
               strip.background=element_rect(fill="grey 80",colour="black"),
               strip.text.x = element_text(size = 7))

stomatal_measurements$CO2 = as.factor(stomatal_measurements$CO2)
#stomatal_measurements$Nutrients = as.factor(stomatal_measurements$Nutrients)
#stomatal_measurements$Response = as.numeric(sub("," , ".", stomatal_measurements$Response))

# Create subsets for all the traits measured
size = subset(stomatal_measurements, Trait == 'Stomatal size')
dense = subset(stomatal_measurements, Trait == 'Stomatal density')
index = subset(stomatal_measurements, Trait == 'Stomatal index')

# Calculate mean, SD, SE and CI for each trait
summary_dense = dense %>%
  group_by(CO2, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(Response),
    sd=sd(Response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

summary_index = index %>%
  group_by(CO2, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(Response),
    sd=sd(Response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

summary_size = size %>%
  group_by(CO2, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(Response),
    sd=sd(Response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# Plot barplots for each trait
size_bar = ggplot(summary_size, aes(x = CO2, y = mean, fill = Nutrients)) +
  geom_bar(position=position_dodge(),stat="identity", width = 0.5) + 
  geom_errorbar(aes(x=CO2, ymin=mean-se, ymax=mean+se), 
                width=0.2, colour="black", alpha=0.9, size=1.0, 
                position=position_dodge(.5)) + 
  labs(x=expression(NULL), y=expression(Stomatal~size~(mu~m))) + 
  scale_fill_manual(values=c('black','lightgray')) + 
  guides(fill=FALSE) + 
  themed + 
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())
  
dense_bar = ggplot(summary_dense, aes(CO2, mean, fill = Nutrients)) +
  geom_bar(position=position_dodge(), stat="identity", width = 0.5) + 
  geom_errorbar(aes(x=CO2, ymin=mean-se, ymax=mean+se),
                width=0.2, colour="black", alpha=0.9, size=1.0, 
                position=position_dodge(.5)) + 
  labs(x=expression(NULL),y=expression(Stomatal~density~(mm^-2))) + 
  scale_fill_manual(values=c('black','lightgray')) +
  themed +
  theme(legend.position = c(0.8, 0.8)) + 
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())

index_bar = ggplot(summary_index, aes(CO2, mean, fill = Nutrients)) +
  geom_bar(position=position_dodge(), stat="identity", width = 0.5) + 
  geom_errorbar(aes(x=CO2, ymin=mean-se, ymax=mean+se),
                width=0.2, colour="black", alpha=0.9, size=1.0, 
                position=position_dodge(.5)) + 
  labs(x=expression(Growth~CO["2"]~~(ppm)),y=expression('Stomatal index (%)')) + 
  scale_fill_manual(values=c('black','lightgray')) + 
  guides(fill=FALSE) + 
  themed

grid.arrange(size_bar, dense_bar, index_bar, nrow = 3)
