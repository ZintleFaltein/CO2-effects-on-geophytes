#' This script takes in a dataframe with photosynthetic values
#' and uses the plantecophys package to return the values of 
#' Jmax, Vcmax and Rd
#' It also plots A-Ci curves for all 3 experiments

# load libraries
library(readxl)
library(plantecophys)
library(tidyverse)
library(ggpmisc)

# load data
aci <- read_excel("C:/Users/User/Documents/GitHub/CO2-effects-on-geophytes/Data/ACi fitting that corresponds to R library plantecophys.xlsx")
aci_plot <- read_excel("C:/Users/User/Documents/GitHub/CO2-effects-on-geophytes/Data/ACi fitting that corresponds to R library plantecophys.xlsx", 
                       sheet = "ACI values for R")
aci_e3 <- read_excel("C:/Users/User/Documents/GitHub/CO2-effects-on-geophytes/Data/ACi fitting that corresponds to R library plantecophys.xlsx", 
                      sheet = "ACI values for R-Exp3")
  
aci$CO2S <- as.numeric(sub("," , ".", aci$CO2S))
aci$Ci <- as.numeric(sub("," , ".", aci$Ci))
aci$Tleaf <- as.numeric(sub("," , ".", aci$Tleaf))
aci$Photo <- as.numeric(sub("," , ".", aci$Photo))

aci_plot$Ave <- as.numeric(sub("," , ".", aci_plot$Ave))
aci_plot$SE <- as.numeric(sub("," , ".", aci_plot$SE))
aci_plot$CO2 <- as.factor(aci_plot$CO2)

aci_e3$Ave <- as.numeric(sub("," , ".", aci_e3$Ave))
aci_e3$SE <- as.numeric(sub("," , ".", aci_e3$SE))
aci_e3$LA <- as.factor(aci_e3$LA)

# create a function to get Vcmax, Jmax and Rd values
fits <- function(exp, spp, LA, rep, co2){
  data = aci%>%
    filter(Expt==exp,
           Spp==as.factor(spp), 
           LA==LA, 
           Rep==rep,
           GCO2==co2) %>%
    fitaci(Tcorrect = FALSE)
  data[2]
  # f= fitaci(data, Tcorrect = FALSE)
  # f[2]
}  

# there's something wrong with this function, it does not work as I expected

#plot A-Ci curves
formula <- y ~ poly(x, 4, raw = TRUE)

# Experiment 2
aci_plot %>%
  filter(Ave >= 0,
         Spp == 'O.pes-caprae',
         CO2 != '400') %>%
  ggplot(aes(x=Ci, y=Ave)) +
  geom_point() + 
  geom_errorbar(aes(ymin=Ave-SE, ymax=Ave+SE)) +
  stat_smooth(method = "lm", formula = formula, se = F, size=0.7, colour = 'black') +
  #stat_poly_eq(
   # aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    #formula = formula) +
  facet_wrap(Spp~CO2, nrow = 1) +
  theme +
  labs(x = expression(Ci~~(mu~mol~mol^-1)), y = expression(A~~(mu~mol~m^-2~s^-1)))

# Experiment 1
aci_plot %>%
  filter(Ave >= 0,
         Spp == 'O.punctata') %>%
  ggplot(aes(x=Ci, y=Ave)) +
  geom_point() + 
  geom_errorbar(aes(ymin=Ave-SE, ymax=Ave+SE)) +
  geom_smooth(method = 'lm', formula = formula, se = F, size=0.7, colour = 'black') +
  # stat_smooth(method = "lm", formula = formula) +
  # stat_poly_eq(
  #   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  #   formula = formula
  # ) +
  facet_wrap(LA~CO2) +
  theme +
  labs(x = expression(Ci~~(mu~mol~mol^-1)), y = expression(A~~(mu~mol~m^-2~s^-1)))

# Experiment 3
aci_e3 %>%
  filter(Ave >= 0) %>%
  ggplot(aes(x=Ci, y=Ave)) +
  geom_point() +
  geom_errorbar(aes(ymin=Ave-SE, ymax=Ave+SE)) +
  geom_smooth(method = 'lm', formula = formula, se = F, size=0.7, colour = 'black') +
  # stat_smooth(method = "lm", formula = formula) +
  # stat_poly_eq(
  #   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  #   formula = formula
  # ) +
  facet_wrap(~factor(LA, levels = c('100% Long Ashton', '70% Long Ashton', '50% Long Ashton'))) +
  theme + theme(strip.text.x = element_text(size = 10)) +
  labs(x = expression(Ci~~(mu~mol~mol^-1)), y = expression(A~~(mu~mol~m^-2~s^-1)))
