# This script plots stacked barplots of epidermal traits with errorobars

library(ggplot2)
library(ggpmisc)
library(dplyr)
library(ggpubr)
library(readr)

leaf_traits <- read_delim("Data/Oxalis leaf traits.csv",";", escape_double = FALSE, trim_ws = TRUE)

theme <- theme(panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               panel.background=element_rect(fill="white"),
               axis.line=element_line("black"),
               panel.border=element_rect(fill=NA,colour="black"),
               strip.background=element_rect(fill="grey 80",colour="black"),
               strip.text.x = element_text(size = 7),
               legend.position = 'right')

leaf_traits$CO2 <- as.factor(leaf_traits$CO2)
leaf_traits$Nutrients = as.factor(leaf_traits$Nutrients)
leaf_traits$Response = as.numeric(sub("," , ".", leaf_traits$Response))
leaf_traits$Trait = as.factor(leaf_traits$Trait)

leaf_traits <- leaf_traits %>%
  filter(Trait != 'Vascular bundle vertical',
         Trait != 'Vascular bundle horizontal')

str(leaf_traits)
head(leaf_traits)

# Calculate the mean, SE and CI for the whole dataset
# Group calculations by treatment and by trait
summary_data <- leaf_traits %>%
  group_by(Nutrients, CO2, Trait) %>%
  summarise( 
    n=n(),
    mean=mean(Response),
    sd=sd(Response)
  ) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))

#  Plot means for high nutrients
leaf_stack <- ggbarplot(leaf_traits, x='CO2', y='Response', 
                        add = 'mean_se', fill = 'Trait',
                        facet.by = 'Nutrients', palette = 'grey', 
                        error.plot = 'errorbar') +
  labs(x=expression(Growth~CO["2"]~~(ppm)),
              y=expression(Leaf~thickness~(mu~m))) +
  theme

# # axis text size and direction
# leaf_stack <- leaf_stack + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# # remove grid lines and set background white
# leaf_stack <- leaf_stack + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# # add black lines around panels
# leaf_stack <- leaf_stack + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# # add strip at top of the figure
# leaf_stack <- leaf_stack + theme(strip.background=element_rect(fill="grey 80",colour="black"))
# leaf_stack <- leaf_stack + theme(legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'))
# # Move legend to the right
# leaf_stack <- leaf_stack + theme(legend.position = 'right')
# leaf_stack
