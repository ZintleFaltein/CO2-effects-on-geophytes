## the data used in this script is from Photosynthetic rates at growth CO2-Exp1&2


library(ggplot2)
library(ggpubr)
library(dplyr)
library(readr)
library(gridExtra)

#photo_stomatal_lim <- read_delim("C:/Users/User/Documents/GitHub/CO2-effects-on-geophytes/Data/Photosynthetic rates at the growth CO2-Exp1&2.csv",
#                                                           delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
#select(response, CO2, Nutrients, Species, comp)

photo_stomatal_lim <-
  read_delim(
    "https://raw.githubusercontent.com/ZintleFaltein/CO2-effects-on-geophytes/master/Data/A%26GS.csv?token=GHSAT0AAAAAAB4K63CPI66M74ZCZLDOSFU4Y4Z5KEA",
    delim = ",",
    escape_double = FALSE,
    trim_ws = TRUE
  )

# update data types
photo_stomatal_lim$response <- as.numeric(sub("," , ".", photo_stomatal_lim$response))
photo_stomatal_lim$CO2 <- as.factor(photo_stomatal_lim$CO2)

# create a theme for the plots
themed <- theme(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_rect(fill = "white"),
  axis.line = element_line("black"),
  panel.border = element_rect(fill = NA, colour = "black"),
  strip.background = element_rect(fill = "grey 80", colour =
                                    "black"),
  strip.text.x = element_text(size = 7)
)

##############################################################################
##                           STOMATAL CONDUCTANCE                            ##
##############################################################################
stomatal_cond_opc = photo_stomatal_lim %>%
  filter(comp == 'Stomatal conductance',
         Species == 'O.pes-caprae',
         response > 0,
         CO2 != 400) %>%
  # ggplot(aes(x=CO2, y=response)) + 
  # geom_boxplot() +
  group_by(CO2, Species) %>%
  summarise(n = n(),
            mean = mean(response),
            sd = sd(response)) %>%
  mutate(se = sd / sqrt(n))  %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + .5, n - 1)) %>%
  ggplot() +
  geom_bar(
    color = 'black',
    fill = 'black',
    aes(x = CO2, y = mean),
    stat = "identity",
    width = 0.6
  ) +
  geom_errorbar(aes(
    x = CO2,
    ymin = mean - se,
    ymax = mean + se
  ), width = 0.1) +
  facet_wrap( ~ Species) +
  labs(x = Growth ~ CO["2"] ~  ~ (ppm), y = expression(Stomatal~conductance~~(mu~mol~m^-2~s^-1))) +
  theme(text = element_text(size = 8)) +
  themed

stomatal_cond_op = photo_stomatal_lim %>%
  filter(comp == 'Stomatal conductance',
         Species == 'O.punctata') %>%
  group_by(CO2, Species, Nutrients) %>%
  summarise(n = n(),
            mean = mean(response),
            sd = sd(response)) %>%
  mutate(se = sd / sqrt(n))  %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + .5, n - 1)) %>%
  ggplot(aes(x = CO2, y = mean, fill = Nutrients)) +
  geom_bar(
    color = 'black',
    stat = "identity",
    width = 0.6,
    position = position_dodge()
  ) +
  geom_errorbar(aes(
    x = CO2,
    ymin = mean - se,
    ymax = mean + se
  ),
  width = 0.1,
  position = position_dodge(0.5)) +
  facet_wrap( ~ Species) +
  labs(x = Growth ~ CO["2"] ~  ~ (ppm), y = NULL) +
  scale_fill_manual(values = c('black', 'grey')) +
  theme(
    legend.position = c(0.8, 0.9),
    legend.key.size = unit(0.5, 'cm'),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)
  ) +
  theme(text = element_text(size = 8)) +
  themed

# combine both plots into 1
grid.arrange(stomatal_cond_opc, stomatal_cond_op, nrow = 1)

##############################################################################
##                           PHOTOSYNTHETIC RATE                            ##
##############################################################################
photo_op = photo_stomatal_lim %>%
  filter(comp == 'Photosynthetic rate',
         Species == 'O.punctata',
         response >0) %>%
  group_by(CO2, Species, Nutrients) %>%
  summarise(n = n(),
            mean = mean(response),
            sd = sd(response)) %>%
  mutate(se = sd / sqrt(n))  %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + .5, n - 1)) %>%
  ggplot(aes(x = CO2, y = mean, fill = Nutrients)) +
  geom_bar(
    color = 'black',
    stat = "identity",
    width = 0.6,
    position = position_dodge()
  ) +
  geom_errorbar(aes(
    x = CO2,
    ymin = mean - se,
    ymax = mean + se
  ),
  width = 0.1,
  position = position_dodge(0.5)) +
  facet_wrap( ~ Species) +
  labs(x = Growth ~ CO["2"] ~  ~ (ppm), y = NULL) +
  scale_fill_manual(values = c('black', 'grey')) +
  theme(
    legend.position = c(0.2, 0.9),
    legend.key.size = unit(0.5, 'cm'),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)
  ) +
  theme(text = element_text(size = 8)) +
  themed   

photo_opc = photo_stomatal_lim %>%
  filter(comp == 'Photosynthetic rate',
         Species == 'O.pes-caprae',
         CO2 != '400',
         response >0) %>%
  group_by(CO2, Species, Nutrients) %>%
  summarise(n = n(),
            mean = mean(response),
            sd = sd(response)) %>%
  mutate(se = sd / sqrt(n))  %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + .5, n - 1)) %>%
  ggplot(aes(x = CO2, y = mean, fill = Nutrients)) +
  geom_bar(
    color = 'black',
    stat = "identity",
    width = 0.6,
    position = position_dodge()
  ) +
  geom_errorbar(aes(
    x = CO2,
    ymin = mean - se,
    ymax = mean + se
  ),
  width = 0.1,
  position = position_dodge(0.5)) +
  facet_wrap( ~ Species) +
  labs(x = Growth ~ CO["2"] ~  ~ (ppm), y = expression(A ~  ~ (mu ~ mol ~
                                                                 m ^ -2 ~ s ^ -1))) +
  scale_fill_manual(values = c('black', 'grey')) +
  theme(legend.position = 'none') +
  theme(text = element_text(size = 8)) +
  themed

# combine both plots into 1
grid.arrange(photo_opc, photo_op, nrow = 1)


##############################################################################
##                           STOMATAL LIMITATION                            ##
##############################################################################
stomatal_lim_opc = photo_stomatal_lim %>%
  filter(comp == 'Stomatal limitation',
         Species == 'O.pes-caprae',
         response > 0 & response < 100,
         CO2 != 400) %>%
  # ggplot(aes(x=CO2, y=response)) + 
  # geom_boxplot() +
  group_by(CO2, Species) %>%
  summarise(n = n(),
            mean = mean(response),
            sd = sd(response)) %>%
  mutate(se = sd / sqrt(n))  %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + .5, n - 1)) %>%
  ggplot() +
  geom_bar(
    color = 'black',
    fill = 'black',
    aes(x = CO2, y = mean),
    stat = "identity",
    width = 0.6
  ) +
  geom_errorbar(aes(
    x = CO2,
    ymin = mean - se,
    ymax = mean + se
  ), width = 0.1) +
  facet_wrap( ~ Species) +
  labs(x = Growth ~ CO["2"] ~  ~ (ppm), y = 'Stomatal limitation (%)') +
  theme(text = element_text(size = 8)) +
  themed

stomatal_lim_op = photo_stomatal_lim %>%
  filter(comp == 'Stomatal limitation',
         Species == 'O.punctata') %>%
  group_by(CO2, Species, Nutrients) %>%
  summarise(n = n(),
            mean = mean(response),
            sd = sd(response)) %>%
  mutate(se = sd / sqrt(n))  %>%
  mutate(ic = se * qt((1 - 0.05) / 2 + .5, n - 1)) %>%
  ggplot(aes(x = CO2, y = mean, fill = Nutrients)) +
  geom_bar(
    color = 'black',
    stat = "identity",
    width = 0.6,
    position = position_dodge()
  ) +
  geom_errorbar(aes(
    x = CO2,
    ymin = mean - se,
    ymax = mean + se
  ),
  width = 0.1,
  position = position_dodge(0.5)) +
  facet_wrap( ~ Species) +
  labs(x = Growth ~ CO["2"] ~  ~ (ppm), y = NULL) +
  scale_fill_manual(values = c('black', 'grey')) +
  theme(
    legend.position = c(0.8, 0.9),
    legend.key.size = unit(0.5, 'cm'),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7)
  ) +
  theme(text = element_text(size = 8)) +
  themed

# combine both plots into 1
grid.arrange(stomatal_lim_opc, stomatal_lim_op, nrow = 1)



##############################################################################
##                                   ANOVA                                  ##
##############################################################################
stom <- photo_stomatal_lim %>%
  filter(comp == 'Stomatal conductance')

lim <- photo_stomatal_lim %>%
  filter(comp == 'Stomatal limitation')

photo <- photo_stomatal_lim %>%
  filter(comp == 'Photosynthetic rate')

#stomatal conductance - O. punctata
conductance.lm<-lm(response ~ CO2*Nutrients, data = filter(stom, Species == 'O.punctata'))
summary(conductance.lm)
conductance.lm<-lm(response ~ CO2, data = filter(stom, Species == 'O.punctata'))
summary(conductance.lm)
conductance.lm<-lm(response ~ Nutrients, data = filter(stom, Species == 'O.punctata'))
summary(conductance.lm)

conductance.aov<-aov(response ~ CO2, data = filter(stom, Species == 'O.punctata'))
TukeyHSD(conductance.aov, conf.level = 0.95)

# photosynthetic values - O. punctata
photo_op.lm<-lm(response ~ CO2*Nutrients, data = filter(photo, Species == 'O.punctata'))
summary(photo_op.lm)
photo_op.lm<-lm(response ~ CO2, data = filter(photo, Species == 'O.punctata'))
summary(photo_op.lm)
photo_op.lm<-lm(response ~ Nutrients, data = filter(photo, Species == 'O.punctata'))
summary(photo_op.lm)

photo_op.aov<-aov(response ~ CO2*Nutrients, data = filter(photo, Species == 'O.punctata'))
TukeyHSD(photo_op.aov, conf.level = 0.95)

#stomatal conductance - O. pes-caprae
conductance_opc.lm<-lm(response ~ CO2, data = filter(stom, Species == 'O.pes-caprae'))
summary(conductance_opc.lm)

conductance_opc.aov<-aov(response ~ CO2, data = filter(stom, Species == 'O.pes-caprae'))
TukeyHSD(conductance_opc.aov, conf.level = 0.95)

photo_opc.lm<-lm(response ~ CO2, data = filter(photo, Species == 'O.pes-caprae'))
summary(photo_opc.lm)

photo_opc.aov<-aov(response ~ CO2, data = filter(photo, Species == 'O.pes-caprae'))
TukeyHSD(photo.aov, conf.level = 0.95)

#stomatal limitation - O. punctata
limitation.lm<-lm(response ~ CO2*Nutrients, data = filter(lim, Species == 'O.punctata'))
summary(limitation.lm)
limitation.lm<-lm(response ~ CO2, data = filter(lim, Species == 'O.punctata'))
summary(limitation.lm)
limitation.lm<-lm(response ~ Nutrients, data = filter(lim, Species == 'O.punctata'))
summary(limitation.lm)

limitation.aov<-aov(response ~ CO2*Nutrients, data = filter(lim, Species == 'O.punctata'))
TukeyHSD(limitation.aov, conf.level = 0.95)
