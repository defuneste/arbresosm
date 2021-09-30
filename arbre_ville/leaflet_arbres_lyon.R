#### un script pour du leaflet sur les arbres de Lyon

## charge les packages

library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)

## un subset pour commancer 

unique(arbres_lyon_final.shp$codeinsee)

quartier <-  arbres_lyon_final.shp %>%
    filter(codeinsee == "69063")
class(quartier)

## les categories de denotation

summary(arbres_lyon_final.shp$localisati)

pal <- colorFactor(palette =c("green", "red", "brown", "blue", "yellow"),
                   levels = c("Espace vert", "Lieu de stationnement", "Terre plein latéral",
                              "TPC ou rond point", "Trottoir ou domaine piétonnier"),
                   na.color = "black")


carto_lyon <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = quartier, radius = 2, 
                     color = ~pal(localisati)) %>%
    addLegend(position = "bottomright",
              pal = pal,
              values = c("Espace vert", "Lieu de stationnement", "Terre plein latéral",
                         "TPC ou rond point", "Trottoir ou domaine piétonnier"),
              na.label = "abs d'info")

carto_lyon





