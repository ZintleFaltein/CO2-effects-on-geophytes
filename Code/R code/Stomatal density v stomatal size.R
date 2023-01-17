# This script creates a scatter plot that explores the relationship
# between stomatal density, conductance and size

library(ggplot2)
library(ggpmisc)
library(gridExtra)
library(tidyverse)
library(readr)

# load data
stomatal_traits <- read_delim('https://raw.githubusercontent.com/ZintleFaltein/CO2-effects-on-geophytes/master/Data/Oxalis%20stomatal%20traits.csv?token=GHSAT0AAAAAAB5TEBPFX3T7XH2VW7DBYHBCY6G2JZA',
                              delim = ';')

themed <- theme(panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               panel.background=element_rect(fill="white"),
               axis.line=element_line("black"),
               panel.border=element_rect(fill=NA,colour="black"),
               strip.background=element_rect(fill="grey 80",colour="black"),
               strip.text.x = element_text(size = 7))

# correct datatypes
stomatal_traits$CO2 <- as.factor(stomatal_traits$CO2)
# stomatal_traits$Nutrients <- as.factor(stomatal_traits$Nutrients)
stomatal_traits$size <- as.numeric(sub("," , ".", stomatal_traits$size))
stomatal_traits$density <- as.numeric(sub("," , ".", stomatal_traits$density))
stomatal_traits$conductance <- as.numeric(sub(",", ".", stomatal_traits$conductance))

str(stomatal_traits)
head(stomatal_traits)

# Plot scatter plot to see correlation between size & density
formula <- y ~ x
stomatal_plot <- ggplot(stomatal_traits, aes(x = density, y = conductance, size = size)) +
  facet_wrap(~Nutrients) +
  geom_point(alpha=0.5) + 
  scale_size(range = c(1, 15), name = expression(Stomata~size~~(mu~m))) +
  geom_smooth(method = "lm", formula = formula, size = 1,color="black",se=FALSE) +
  stat_poly_eq(formula = formula, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                      parse = TRUE, size=3) +
  labs(x=expression(Stomatal~density~~(mm^-2)),y=expression(Stomatal~conductance~~(mu~mol~m^-2~s^-1))) +
  themed 

stomatal_plot

#stomatal_plot <- stomatal_plot + scale_shape_manual(values = c(3, 16, 8)) + 
  #scale_size_manual(values = c(10, 10, 10))
#stomatal_plot <- stomatal_plot + scale_colour_manual(values = c("black", "red", 'blue'))
# Show equation and R squared value on the plot

#Removes legend
#stomatal_plot <- stomatal_plot + guides(fill=FALSE)
#Axis text size and direction
# stomatal_plot <- stomatal_plot + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# #Remove grid lines and set background white
# stomatal_plot <- stomatal_plot +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# #Black lines around panels
# stomatal_plot <- stomatal_plot+ theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# #Strip at top of fig
# stomatal_plot <-stomatal_plot + theme(strip.background=element_rect(fill="grey 80",colour="black"))
# stomatal_plot <- stomatal_plot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), panel.spacing = unit(0.2, 'lines'))
# 
# #Plot relationship between stomatal size and conductance
# formula = y ~ x
# density_conductance <- ggplot(stomatal_traits, aes(x = density, y = conductance))
# density_conductance <- density_conductance + facet_wrap(~Nutrients, )
# density_conductance <- density_conductance + geom_point() + scale_shape_manual(values = c(3, 16, 8)) + 
#   scale_size_manual(values = c(10, 10, 10)) +
#  geom_smooth(data=stomatal_traits,method = "lm", formula = formula, size = 1,color="black",se=TRUE)
# #p <- p + scale_colour_manual(values = c("black", "red", 'blue'))
# # Show equation and R squared value on the plot
# density_conductance <- density_conductance + stat_poly_eq(formula = formula, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size=3)
# density_conductance
# 
# density_conductance <- density_conductance + labs(x=expression(Stomatal~density~~(mm^-2)),y=expression(Stomatal~conductance~~(mu~mol~m^-2~s^-1)))
# 
# #Removes legend
# density_conductance <- density_conductance + guides(fill=FALSE)
# #Axis text size and direction
# density_conductance <- density_conductance + theme(text = element_text(size=10),axis.text.x = element_text(angle=0, vjust=1,color="black"),axis.text.y = element_text(color="black"))
# #Remove grid lines and set background white
# density_conductance <- density_conductance +theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))
# #Black lines around panels
# density_conductance <- density_conductance + theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black"))
# #Strip at top of fig
# density_conductance <- density_conductance + theme(strip.background=element_rect(fill="grey 80",colour="black"))
# density_conductance <- density_conductance + theme(panel.spacing = unit(0.2, 'lines'))
# 
# grid.arrange(stomatal_plot, density_conductance, ncol = 1)
