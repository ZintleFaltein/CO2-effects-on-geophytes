
library(ggplot2)
library(dplyr)
library(plotrix)
library(gridExtra)
library(tidyverse)
library(readr)
library(curl)

returns <- read_delim("C:/Users/User/Documents/GitHub/CO2-effects-on-geophytes/R code/Data/Return rates by veg type.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

returns

returns$Cal_per_hr = as.numeric(sub("," , ".", returns$Cal_per_hr))

head(returns)


themed <- theme(panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               panel.background=element_rect(fill="white"),
               axis.line=element_line("black"),
               panel.border=element_rect(fill=NA,colour="black"),
               strip.background=element_rect(fill="grey 80",colour="black"),
               strip.text.x = element_text(size = 7))
returns %>%
  filter(Cal_per_hr >= 1000) %>%
  #filter(`Vegetation Type` == c('Dune fynbos-thicket mos.', 'Sand fynbos', 'Riparian')) %>%
  ggplot(aes(x=CO2, y = Cal_per_hr, group = CO2)) + 
  geom_boxplot(outlier.colour = 'red') +
  facet_wrap(~`Vegetation Type`) +
  themed
