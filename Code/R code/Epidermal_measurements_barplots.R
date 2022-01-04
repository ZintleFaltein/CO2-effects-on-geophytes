
leaf_traits <- read_delim("Data/Oxalis leaf traits.csv",";", escape_double = FALSE, trim_ws = TRUE)

library(ggplot2)
library(ggpmisc)
library(dplyr)
library(gridExtra)

leaf_traits$CO2 = as.factor(leaf_traits$CO2)
leaf_traits$Nutrients = as.factor(leaf_traits$Nutrients)
leaf_traits$Response = as.numeric(sub("," , ".", leaf_traits$Response))
leaf_traits$Trait = as.factor(leaf_traits$Trait)

leaf_thickness = subset(leaf_traits, Trait == 'Leaf thickness')
palisade = subset(leaf_traits, Trait == 'Palisade length')
mesophyll = subset(leaf_traits, Trait == 'Mesophyll length')
upper_epidermis = subset(leaf_traits, Trait == 'Upper epidermis')
lower_epidermis = subset(leaf_traits, Trait == 'Lower epidermis')

summary_lt = leaf_thickness %>%
  group_by(CO2, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(Response),
    sd=sd(Response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

summary_palisade = palisade %>%
  group_by(CO2, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(Response),
    sd=sd(Response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

summary_mesophyll = mesophyll %>%
  group_by(CO2, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(Response),
    sd=sd(Response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

summary_upper = upper_epidermis %>%
  group_by(CO2, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(Response),
    sd=sd(Response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

summary_lower = lower_epidermis %>%
  group_by(CO2, Nutrients) %>%
  summarise( 
    n=n(),
    mean=mean(Response),
    sd=sd(Response)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# Plot barplots for each trait
lt_bar = ggplot(summary_lt, aes(x = CO2, y = mean, fill = Nutrients)) + 
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(x = CO2, ymin = mean-se, ymax = mean+se), width = 0.2, colour = "black", alpha=0.9, size=0.5, position = position_dodge(0.7)) +
  scale_fill_manual(values = c('black', 'grey'))

# Change axis labels
lt_bar = lt_bar + labs(x=expression(Growth~CO["2"]~~(ppm)),
                           y=expression(LT~(mu~m)))

# remove legend
lt_bar = lt_bar + guides(fill=FALSE)
# axis text size and direction
lt_bar = lt_bar + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
lt_bar= lt_bar + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
lt_bar= lt_bar + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
lt_bar= lt_bar + theme(strip.background=element_rect(fill="grey 80",colour="black"))
#lt_bar


upper_bar = ggplot(summary_upper, aes(x = CO2, y = mean, fill = Nutrients)) + 
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(x = CO2, ymin = mean-se, ymax = mean+se), width = 0.2, colour = "black", alpha=0.9, size=0.5, position = position_dodge(0.7)) +
  scale_fill_manual(values = c('black', 'grey'))

# Change axis labels
upper_bar = upper_bar + labs(x=expression(' '),
                       y=expression(UET~(mu~m)))

# remove legend
upper_bar = upper_bar + guides(fill=FALSE)
# axis text size and direction
upper_bar = upper_bar + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
upper_bar= upper_bar + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
upper_bar= upper_bar + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
upper_bar= upper_bar + theme(strip.background=element_rect(fill="grey 80",colour="black"))
#upper_bar

lower_bar = ggplot(summary_lower, aes(x = CO2, y = mean, fill = Nutrients)) + 
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(x = CO2, ymin = mean-se, ymax = mean+se), width = 0.2, colour = "black", alpha=0.9, size=0.5, position = position_dodge(0.7)) +
  scale_fill_manual(values = c('black', 'grey'))

# Change axis labels
lower_bar = lower_bar + labs(x=expression(' '),
                                   y=expression(LET~(mu~m)))

# remove legend
#lower_bar = lower_bar + guides(fill=FALSE)
# axis text size and direction
lower_bar = lower_bar + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
lower_bar= lower_bar + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
lower_bar= lower_bar + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
lower_bar= lower_bar + theme(strip.background=element_rect(fill="grey 80",colour="black"))
#lower_bar

lower_bar = lower_bar + theme(
  legend.position = c(1, 1), 
  legend.justification = c('right', 'top'), 
  legend.box.just = 'right', 
  legend.margin = margin(6, 6, 6, 6),
  legend.key.size = unit(0.5, 'cm'),
  legend.background = element_rect(fill = 'transparent'),
  legend.title = element_text(size = 7),
  legend.text = element_text(size = 7)
)

mesophyll_bar = ggplot(summary_mesophyll, aes(x = CO2, y = mean, fill = Nutrients)) + 
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(x = CO2, ymin = mean-se, ymax = mean+se), width = 0.2, colour = "black", alpha=0.9, size=0.5, position = position_dodge(0.7)) +
  scale_fill_manual(values = c('black', 'grey'))

# Change axis labels
mesophyll_bar = mesophyll_bar + labs(x=expression(' '),
                                   y=expression(SMT~(mu~m)))

# remove legend
mesophyll_bar = mesophyll_bar + guides(fill=FALSE)
# axis text size and direction
mesophyll_bar = mesophyll_bar + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
mesophyll_bar= mesophyll_bar + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
mesophyll_bar= mesophyll_bar + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
mesophyll_bar= mesophyll_bar + theme(strip.background=element_rect(fill="grey 80",colour="black"))
#mesophyll_bar

palisade_bar = ggplot(summary_palisade, aes(x = CO2, y = mean, fill = Nutrients)) + 
  geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
  geom_errorbar(aes(x = CO2, ymin = mean-se, ymax = mean+se), width = 0.2, colour = "black", alpha=0.9, size=0.5, position = position_dodge(0.7)) +
  scale_fill_manual(values = c('black', 'grey'))

# Change axis labels
palisade_bar = palisade_bar + labs(x=expression(' '),
                                   y=expression(PMT~(mu~m)))

# remove legend
palisade_bar = palisade_bar + guides(fill=FALSE)
# axis text size and direction
palisade_bar = palisade_bar + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
palisade_bar= palisade_bar + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
palisade_bar= palisade_bar + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
palisade_bar= palisade_bar + theme(strip.background=element_rect(fill="grey 80",colour="black"))
#palisade_bar

grid.arrange(upper_bar, lower_bar, palisade_bar, mesophyll_bar, lt_bar, ncol = 1)
