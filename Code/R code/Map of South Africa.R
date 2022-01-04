library(ggplot2)
#theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(ggspatial)
library(ggdraw)

world<- ne_countries(scale = "medium", returnclass = "sf")


#Plot the whole map of South Africa with the coordinates given in ylim and xlim

main <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(14, 34), ylim = c(-36, -22), expand = FALSE) + 
  annotation_scale(location = "br", width_hint = 0.2, height =unit(0.1, "cm")) +
  annotation_north_arrow(location = "br", which_north = "true", width = unit(1, "cm"),
                         height = unit(1, "cm"),
                         pad_x = unit(0.7, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) + 
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))

#Plot a map of the CFR with the coordinates given in ylim and xlim
cfr<- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(17, 26), ylim = c(-35, -31), expand = FALSE) + 
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background=element_rect(fill="white"))

#Add annotations/text
main + annotate("text", x=31, y=-32, label="INDIAN OCEAN", size=2.5) + 
  annotate("text", x=24, y=-29, label="SOUTH AFRICA") + 
  annotate("text", x=16, y=-32, label="ATLANTIC OCEAN", size=2.5) 
  #theme(axis.line=element_line("black"),panel.border=element_rect(fill=NA,colour="black")) 

#Plot both the map of SA and the inset (CFR) on the same set of axes
ggdraw(main) + draw_plot(cfr, 0.6, 0.6, 0.5, 0.5)
