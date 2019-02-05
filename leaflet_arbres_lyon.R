#### un script pour du leaflet sur les arbres de Lyon

## charge les packages

library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)

## un subset pour commancer 

unique(arbres_lyon_final.shp$codeinsee)

quartier <- arbres_lyon_final.shp %>%
    filter(codeinsee == "69003")
class(quartier)

carto_lyon <- leaflet() %>%
    addTiles() %>%
    addMarkers(data = quartier)