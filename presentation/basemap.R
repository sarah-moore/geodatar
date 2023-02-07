library(leaflet)
library(tidyverse)
library(htmlwidgets)
library(mapview)
setwd("~/OneDrive - Northwestern University/Teaching_RA/geodatar")

leaflet() %>%  
  addProviderTiles("Esri.WorldTopoMap") %>%
  setView(lat = -14, lng = -51, zoom= 4)-> title_map

mapshot(title_map, file = "images/title_map.png")
