## the data used in this script is from Photosynthetic rates at growth CO2-Exp1&2


library(ggplot2)
library(ggpubr)
library(dplyr)
library(readr)

photo_stomatal_lim <- read_delim("C:/Users/User/Documents/GitHub/CO2-effects-on-geophytes/Data/Photosynthetic rates at the growth CO2-Exp1&2.csv", 
                                                            delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  select(response, CO2, Nutrients, Species, comp)

# update data types
photo_stomatal_lim$response <- as.numeric(sub("," , ".", photo_stomatal_lim$response))
photo_stomatal_lim$CO2 <- as.factor(photo_stomatal_lim$CO2)

# create a theme for the plots
themed <- theme(panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                panel.background=element_rect(fill="white"),
                axis.line=element_line("black"),
                panel.border=element_rect(fill=NA,colour="black"),
                strip.background=element_rect(fill="grey 80",colour="black"),
                strip.text.x = element_text(size = 7))

stomatal_lim_opc = photo_stomatal_lim %>%
  filter(comp == 'Stomatal limitation',
         Species == 'O.pes-caprae') %>%
  group_by(CO2, Species) %>%
  summarise( 
    n=n(),
    mean=mean(response),
    sd=sd(response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  ggplot() +
  geom_bar(color = 'black', fill = 'black', 
           aes(x=CO2, y=mean), 
           stat="identity", width = 0.6) +
  geom_errorbar(aes(x=CO2, ymin=mean-se, ymax=mean+se), width=0.1) +
  facet_wrap(~Species) +
  labs(x = Growth~CO["2"]~~(ppm), y = 'Stomatal limitation (%)' ) +
  themed

stomatal_lim_op = photo_stomatal_lim %>%
  filter(comp == 'Stomatal limitation',
         Species == 'O.punctata') %>%
  group_by(CO2, Species, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(response),
    sd=sd(response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  ggplot() +
  geom_bar(color = 'black',  
           aes(x=CO2, y=mean, fill = Nutrients), 
           stat="identity", width = 0.6, position = position_dodge()) +
  geom_errorbar(aes(x=CO2, ymin=mean-se, ymax=mean+se), width=0.1, position = position_dodge(0.5)) +
  labs(x = Growth~CO["2"]~~(ppm), y = 'Stomatal limitation (%)' ) +
  scale_fill_manual(values = c('black', 'grey')) +
  themed

summary_photo_stomatal_lim_photo = photo_stomatal_lim %>%
  group_by(CO2.concentration) %>%
  summarise( 
    n=n(),
    mean=mean(A...Ca.180),
    sd=sd(A...Ca.180)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  ggplot() +
  geom_bar(color = 'black', fill = 'black', 
           aes(x=CO2.concentration, y=mean), 
           stat="identity", width = 0.6) +
  geom_errorbar(aes(x=CO2.concentration, ymin=mean-se, ymax=mean+se), width=0.1) +
  labs(x = Growth~CO["2"]~~(ppm), y=expression(A~~(mu~mol~m^-2~s^-1))) +
  themed
