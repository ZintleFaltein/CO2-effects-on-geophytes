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
aci <-
  read_excel(
    "C:/Users/User/Documents/GitHub/CO2-effects-on-geophytes/Data/ACi fitting that corresponds to R library plantecophys.xlsx"
  )
aci_plot <-
  read_excel(
    "C:/Users/User/Documents/GitHub/CO2-effects-on-geophytes/Data/ACi fitting that corresponds to R library plantecophys.xlsx",
    sheet = "ACI values for R-Exp1&2"
  )
# aci_e3 <-
#   read_excel(
#     "C:/Users/User/Documents/GitHub/CO2-effects-on-geophytes/Data/ACi fitting that corresponds to R library plantecophys.xlsx",
#     sheet = "ACI values for R-Exp3"
#   )

# correct the data types for CO2, leaf temperature & photosynthesis
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

# create a function to get A an gst at growth CO2
fits_ca <- function(exp, LA, rep, co2, fitmethod = 'default') {
  #' Take in a data frame and fit the ACI curve
  #'
  #' @param Expt  Experiment number
  #' @param LA Nutrient concentration
  #' @param Rep  Replicate
  #' @param GCO2 Growth [CO2] applied to the plants
  #' @param fitmethod Method to fit the ACi curve
  #' @return A and GS at growth CO2
  data = aci %>%
    filter(Expt == exp,
           #Spp==as.factor(spp),
           LA == LA,
           Rep == rep,
           GCO2 == co2) %>%
    fitaci(Tcorrect = FALSE, fitmethod = fitmethod)
  #ci = Photosyn(Ca = co2)
  return(data$Photosyn(Ca = co2)[2:3])
  # f= fitaci(data, Tcorrect = FALSE)
  # f[2]
}

# create a function to get A at Ci=CO2
fits_ci <- function(exp, LA, rep, co2, fitmethod = 'default') {
  #' Take in a data frame and fit the ACI curve
  #'
  #' @param Expt  Experiment number
  #' @param LA Nutrient concentration
  #' @param Rep  Replicate
  #' @param GCO2 Growth [CO2] applied to the plants
  #' @param fitmethod Method to fit the ACi curve
  #' @return A and GS at growth CO2
  data = aci %>%
    filter(Expt == exp,
           #Spp==as.factor(spp),
           LA == LA,
           Rep == rep,
           GCO2 == co2) %>%
    fitaci(Tcorrect = FALSE, fitmethod = fitmethod)
  #ci = Photosyn(Ca = co2)
  return(data$Photosyn(Ci = co2)[2])
  # f= fitaci(data, Tcorrect = FALSE)
  # f[2]
}

###########################################################
#                   plot A-Ci curves                      #
###########################################################

# get the formula for the A-ci curve
formula <- y ~ poly(x, 4, raw = TRUE)

# plot the A-ci curves for each experiment
# Experiment 2
aci_plot %>%
  filter(Ave >= 0,
         Spp == 'O.pes-caprae',
         CO2 != '400') %>%
  ggplot(aes(x = Ci, y = Ave)) +
  geom_point() +
  geom_errorbar(aes(ymin = Ave - SE, ymax = Ave + SE)) +
  stat_smooth(
    method = "lm",
    formula = formula,
    se = F,
    size = 0.7,
    colour = 'black'
  ) +
  #stat_poly_eq(
  # aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  #formula = formula) +
  #geom_vline(xintercept = c(180, 240, 300)) +
  facet_wrap(Spp~CO2, nrow = 1) +
 #facet_grid(rows = vars(Spp), cols = vars(CO2)) +
  theme +
  labs(x = expression(Ci ~  ~ (mu ~ mol ~ mol ^ -1)), y = expression(A ~
                                                                       ~ (mu ~ mol ~ m ^ -2 ~ s ^ -1)))

# find the photosynthetic rate at Ci=Ca
# loop through all the replicates to get A and GS using the function fits
for (i in 1:5) {
  print(fits_ca(2, 100, i, 180))
}

for (i in 1:5) {
  print(fits_ci(2, 100, i, 180))
}

for (i in 1:5) {
  print(fits_ca(2, 100, i, 240))
}

for (i in 1:5) {
  print(fits_ci(2, 100, i, 240))
}

for (i in 1:5) {
  print(fits_ca(2, 100, i, 300))
}

for (i in 1:5) {
  print(fits_ci(2, 100, i, 300))
}


#for (i in 1:3) {
 # print(fits(2, 100, i, 400))
#}

# Experiment 1
aci_plot %>%
  filter(Ave >= 0,
         Spp == 'O.punctata') %>%
  ggplot(aes(x = Ci, y = Ave)) +
  geom_point() +
  geom_errorbar(aes(ymin = Ave - SE, ymax = Ave + SE)) +
  geom_smooth(
    method = 'lm',
    formula = formula,
    se = F,
    size = 0.7,
    colour = 'black'
  ) +
  # stat_smooth(method = "lm", formula = formula) +
  # stat_poly_eq(
  #   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  #   formula = formula
  # ) +
  #facet_wrap(LA~CO2) +
  #geom_vline(xintercept = c(180, 280, 400)) +
  facet_wrap(Spp ~ LA + CO2) +
  theme +
  labs(x = expression(Ci ~  ~ (mu ~ mol ~ mol ^ -1)), y = expression(A ~
                                                                       ~ (mu ~ mol ~ m ^ -2 ~ s ^ -1)))

# find the photosynthetic rate at Ci=Ca
# loop through all the replicates to get A and GS
for (i in 1:3) {
  print(fits_ca(1, 50, i, 180, 'bilinear'))
}

for (i in 1:3) {
  print(fits_ci(1, 50, i, 180, 'bilinear'))
}


for (i in 1:3) {
  print(fits_ca(1, 50, i, 280))
}

for (i in 1:3) {
  print(fits_ci(1, 50, i, 280))
}

for (i in 1:3) {
  print(fits_ca(1, 50, i, 400))
}

for (i in 1:3) {
  print(fits_ci(1, 50, i, 400))
}

for (i in 1:3) {
  print(fits_ca(1, 100, i, 180, 'bilinear'))
}

for (i in 1:3) {
  print(fits_ci(1, 100, i, 180, 'bilinear'))
}

for (i in 1:3) {
  print(fits_ca(1, 100, i, 280))
}

for (i in 1:3) {
  print(fits_ci(1, 100, i, 280))
}

for (i in 1:3) {
  print(fits_ca(1, 100, i, 400))
}

for (i in 1:3) {
  print(fits_ci(1, 100, i, 400))
}

# Experiment 3
aci_e3 %>%
  filter(Ave >= 0) %>%
  ggplot(aes(x = Ci, y = Ave)) +
  geom_point() +
  geom_errorbar(aes(ymin = Ave - SE, ymax = Ave + SE)) +
  geom_smooth(
    method = 'lm',
    formula = formula,
    se = F,
    size = 0.7,
    colour = 'black'
  ) +
  # stat_smooth(method = "lm", formula = formula) +
  # stat_poly_eq(
  #   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  #   formula = formula
  # ) +
  facet_wrap( ~ factor(
    LA,
    levels = c('100% Long Ashton', '70% Long Ashton', '50% Long Ashton')
  )) +
  theme + theme(strip.text.x = element_text(size = 10)) +
  labs(x = expression(Ci ~  ~ (mu ~ mol ~ mol ^ -1)), y = expression(A ~
                                                                       ~ (mu ~ mol ~ m ^ -2 ~ s ^ -1)))



# find the photosynthetic rate at Ci=Ca
# loop through all the replicates to get A and GS
# for (i in 1:4) {
#   print(fits_ca3(3, 100, i, 'bilinear'))
# }
# 
# for (i in 1:4) {
#   print(fits_ci(3, 100, i, 400, 'bilinear'))
# }
# 
# for (i in 1:5) {
#   print(fits_ca(3, 70, i, 400, 'bilinear'))
# }
# 
# for (i in 1:5) {
#   print(fits_ci(3, 70, i, 400, 'bilinear'))
# }
# 
# 
# for (i in 1:4) {
#   print(fits_ca(3, 50, i, 400, 'bilinear'))
# }
# 
# for (i in 1:4) {
#   print(fits_ci(3, 50, i, 400, 'bilinear'))
# }

###### The function does not work for the above code so I used the code below instead

# find photosynthetic values for Exp3
data = aci %>%
  filter(Expt == 3,
         LA == 70,
         Rep == 5) %>%
  fitaci(Tcorrect = FALSE, fitmethod = 'bilinear')
data$Photosyn(Ca=400)
data$Photosyn(Ci=400)
