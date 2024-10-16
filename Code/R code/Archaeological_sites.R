library(leaflet)
library(dplyr)
library(tidyverse)
library(readxl)

sites <- read_excel("Data/Foraging sites.xlsx", 
                    col_types = c("text", "numeric", "numeric"))
View(sites)

mybins <- seq(4, 6.5, by=0.5)
mypalette <- colorBin( palette="YlOrBr", na.color="transparent", bins=mybins)

sites_map <- sites %>%
  filter(site != 'Boegboeberg 1') %>%
  leaflet() %>% 
  addTiles()  %>% 
  setView( lat=-34, lng=21 , zoom=6) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addCircleMarkers(~lon, ~lat, 
                   fillOpacity = 0.7, color="black", radius=8, stroke=FALSE, 
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>%
  addScaleBar(position = 'bottomright') %>%
  addMiniMap(tiles = providers$Esri.WorldGrayCanvas)

sites_map
