
library(ggplot2)
library(dplyr)
library(plotrix)
library(gridExtra)
library(tidyverse)
library(readr)
library(curl)
library(reshape)
library(ggridges)

# returns <- read_delim("https://raw.githubusercontent.com/ZintleFaltein/CO2-effects-on-geophytes/master/R%20code/Data/Foraging%20return%20rates.csv?token=AOMLEIHYF2HMAOH6ZEDAGTLBYTLX6", 
#                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

returns <- read_delim("https://raw.githubusercontent.com/ZintleFaltein/CO2-effects-on-geophytes/master/R%20code/Data/Return%20rates%20by%20veg%20type.csv?token=AOMLEIAEMBMUQDJ3HV6ET4DB2XDNE",
                      delim = ';')

View(returns)

returns$Cal_per_hr = as.numeric(sub("," , ".", returns$Cal_per_hr))

glimpse(returns)

themed <- theme(panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               panel.background=element_rect(fill="white"),
               axis.line=element_line("black"),
               panel.border=element_rect(fill=NA,colour="black"),
               strip.background=element_rect(fill="grey 80",colour="black"),
               strip.text.x = element_text(size = 7))

# returns <- returns %>%
#   select(`Vegetation Type`, `BOUT ID`, `return(400)`, 
#          `return(300)`, `return(240)`, `return(180)`) 

#colnames(returns) <- c('veg_type', 'bout_id', '400', '300', '240', '180')

# melt the dataframe
# returns <- melt(returns, id.vars = c('veg_type', 'bout_id'),
#                 measure.vars = c('400', '300', '240', '180'))


if (returns$`Vegetation Type` == 'Dune fynbos-thicket mos.'){
  
}

# get the 90% quantile of calorific values
returns %>%
  filter(Cal_per_hr >= quantile(returns$Cal_per_hr, .90)) 

box_calories <- returns %>%
  #filter(Cal_per_hr >= 1000) %>%
  filter(Cal_per_hr > quantile(returns$Cal_per_hr, .90)) %>%
  #filter(`Vegetation Type` %in% c('Dune fynbos-thicket mos.', 'Sand fynbos', 'Riparian')) %>%
  ggplot(aes(x=CO2, y = Cal_per_hr, group = CO2)) + 
  geom_boxplot(outlier.colour = 'red') +
 # facet_wrap(~`Vegetation Type`) +
  themed +
  labs(x=expression(Growth~CO["2"]~~(ppm)), y = 'Return rates (Cal/hr)')

box_calories

# plot ridge plots to see the distribution
# of calories per hour harvested
ridge_calories <- returns %>%
  #filter(Cal_per_hr >= 500) %>%
  #filter(`Vegetation Type` %in% c('Dune fynbos-thicket mos.', 'Sand fynbos', 'Riparian')) %>%
  filter(Cal_per_hr > quantile(returns$Cal_per_hr, .90)) %>%
  ggplot(aes(x=Cal_per_hr, y=`Vegetation Type`)) +
  geom_density_ridges() +
  theme_ridges() +
  facet_wrap(~CO2) +
  geom_vline(xintercept = 2000, color = 'red', linetype = 'dashed') +
  themed

ridge_calories
