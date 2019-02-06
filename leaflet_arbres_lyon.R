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
    addCircleMarkers(data = quartier, radius = 2, 
                     color = ~pal(localisati))

carto_lyon

## les categories de denotation

summary(arbres_lyon_final.shp$localisati)

pal <- colorFactor(palette =c("green", "red", "brown", "grey", "pink"),
                   levels = c("Espace vert", "Lieu de stationnement ", "Terre plein latéral",
                              "TPC ou rond point", "Trottoir ou domaine piétonnier"))



