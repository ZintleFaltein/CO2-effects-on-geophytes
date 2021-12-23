
library(ggplot2)
library(dplyr)
library(plotrix)
library(gridExtra)
library(tidyverse)
library(readr)
library(curl)
library(reshape)

returns <- read_delim("https://raw.githubusercontent.com/ZintleFaltein/CO2-effects-on-geophytes/master/R%20code/Data/Foraging%20return%20rates.csv?token=AOMLEIHYF2HMAOH6ZEDAGTLBYTLX6", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

View(returns)

#returns$Cal_per_hr = as.numeric(sub("," , ".", returns$Cal_per_hr))

themed <- theme(panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               panel.background=element_rect(fill="white"),
               axis.line=element_line("black"),
               panel.border=element_rect(fill=NA,colour="black"),
               strip.background=element_rect(fill="grey 80",colour="black"),
               strip.text.x = element_text(size = 7))
returns <- returns %>%
  select(`Vegetation Type`, `BOUT ID`, `return(400)`, 
         `return(300)`, `return(240)`, `return(180)`) 

# melt the dataframe
returns <- melt(returns, id.vars = c(`Vegetation Type`, `BOUT ID`),
                measure.vars = c(`return(400)`,`return(300)`, 
                                 `return(240)`, `return(180)`))

return %>%
  filter(Cal_per_hr >= 1000) %>%
  #filter(`Vegetation Type` == c('Dune fynbos-thicket mos.', 'Sand fynbos', 'Riparian')) %>%
  ggplot(aes(x=CO2, y = Cal_per_hr, group = CO2)) + 
  geom_boxplot(outlier.colour = 'red') +
  facet_wrap(~`Vegetation Type`) +
  themed
