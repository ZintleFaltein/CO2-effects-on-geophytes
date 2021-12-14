# This script plots a side by side barplot of epidermal traits. The total is the leaf thickness

data <- read.delim(file = "clipboard", sep = "\t")

library(ggplot2)
library(ggpmisc)
library(dplyr)

data$CO2 <- as.factor(data$CO2)
data$Nutrients = as.factor(data$Nutrients)
data$Response = as.numeric(sub("," , ".", data$Response))
data$Trait = as.factor(data$Trait)

str(data)
head(data)

# Calculate the mean, SE and CI for the whole dataset
# Group calculations by treatment and by trait
summary_data <- data %>%
       group_by(CO2, Nutrients) %>%
       summarise( 
             n=n(),
             mean=mean(Response),
             sd=sd(Response)
         ) %>%
       mutate( se=sd/sqrt(n))  %>%
       mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

#  Plot stacked barplot and facet by nutrients
p <- ggplot(summary_data, aes(x = CO2, y = mean, fill = Trait)) + 
  geom_bar(stat = "identity", width = 0.9, position = position_dodge()) +
  geom_errorbar(aes(x = CO2, ymin = mean-se, ymax = mean+se), width = 0.2, 
                colour = "black", alpha=0.9, size=0.5, position = position_dodge(0.8)) + 
  facet_wrap(~Nutrients, nrow = 2) + scale_fill_brewer(palette = 'RdGy')

p <- p + labs(x=expression(' '),
              y=expression(Length~(mu~m)))

# axis text size and direction
p <- p + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
p <- p + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
p <- p + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
p <- p + theme(strip.background=element_rect(fill="grey 80",colour="black"))
p <- p + theme(legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'))
p

# Plot leaf thickness
g <- ggplot(summary_data, aes(x = CO2, y = mean, fill = Nutrients)) + 
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) +
  geom_errorbar(aes(x = CO2, ymin = mean-se, ymax = mean+se), width = 0.2, 
                colour = "black", alpha=0.9, size=0.5, position = position_dodge(0.5)) + 
  scale_fill_manual(values = c('black', 'grey'))

g <- g + labs(x=expression(Growth~CO["2"]~~(ppm)),
              y=expression(Leaf~thickness~(mu~m)))

# axis text size and direction
g <- g + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# remove grid lines and set background white
g <- g + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# add black lines around pannels
g <- g + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# add strip at top of the figure
g <- g + theme(strip.background=element_rect(fill="grey 80",colour="black"))
g <- g + theme(legend.title = element_text(size = 8), legend.key.size = unit(0.4, 'cm'))
g

# Combine plots
grid.arrange(p, g, ncol = 1)
