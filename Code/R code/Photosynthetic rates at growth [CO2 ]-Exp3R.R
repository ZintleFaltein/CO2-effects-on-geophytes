library(gridExtra)
library(ggplot2)

a$A_Ca.400 <- as.numeric(sub("," , ".", a$A_Ca.400))
a$A_Ci.400 <- as.numeric(sub("," , ".", a$A_Ci.400))
a$Stomatal.limitation<- as.numeric(sub("," , ".", a$Stomatal.limitation))
a$Nutrients <- as.factor(a$Nutrients)

photo = a %>%
  group_by(Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(A_Ca.400),
    sd=sd(A_Ca.400)
  ) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  ggplot() +
  geom_bar(color = 'black', fill = 'black', 
           aes(x=Nutrients, y=mean), 
           stat="identity", width = 0.6) +
  geom_errorbar(aes(x=Nutrients, ymin=mean-se, ymax=mean+se), width=0.1) +
 # facet_wrap(~Species) +
  labs(x = 'Nutrient concentration (%)', y=expression(A~~(mu~mol~m^-2~s^-1))) +
  theme(text = element_text(size=8)) +
  themed

lim = a %>%
  group_by(Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(Stomatal.limitation),
    sd=sd(Stomatal.limitation)
  ) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1)) %>%
  ggplot() +
  geom_bar(color = 'black', fill = 'black', 
           aes(x=Nutrients, y=mean), 
           stat="identity", width = 0.6) +
  geom_errorbar(aes(x=Nutrients, ymin=mean-se, ymax=mean+se), width=0.1) +
  # facet_wrap(~Species) +
  labs(x = 'Nutrient concentration (%)', y = 'Stomatal limitation (%)') +
  theme(text = element_text(size=8)) +
  themed

grid.arrange(photo, lim, ncol = 1)
