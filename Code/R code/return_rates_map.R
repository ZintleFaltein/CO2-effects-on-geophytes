# plot foraging areas and return rates on a map
library(leaflet)
library(readxl)

coord <- read_excel("C:/Users/User/OneDrive - Rhodes University/Documents/Thesis/Foraging return rates.xlsx", sheet = "kcal per bout", n_max = 80)

View(coord)

coord$Lat <- as.numeric(coord$Lat)
coord$Lon <- as.numeric(coord$Lon)

leaflet(coord) %>% 
  addTiles() %>% 
  setView(lng=21, lat = -34, zoom=8) %>% 
  addCircleMarkers(~Lon, ~Lat)

