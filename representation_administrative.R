### projet pour l'analyse des arbres seul dans OSM 
# exploration et "nettoyage des données"
# Petites notes sur le code :
# pour me rapeller qu'un objet à des infos geometriques type vecteur je lui accolle un .shp
# si c'est un raster .grid, si c'est un dataframe souvent un .dat

# chargement des différents packages demandés ===========

### DB
library(RPostgreSQL) # fait le lien avec postgre, utilise DBI

### manip
library(dplyr) # manip de données en tidyverse
library(tibble)
library(tidyr)
library(lubridate) # date
library(stringr) # modif sur character

### visualisation
library(ggplot2) # la visualisation
library(tmap) # carto
library(ggmap)# carto +
library(leaflet) # carto web
library(rsconnect) # pour partager une carte

## analyse spatiale / carto
library(sp) # classes et methodes pour données spatiales pe déclassé par SF
library(rgdal) #gdal pour projection, crud et surtout export
library(rgeos) # geos, penser à installer libgeos++-dev avant, travail avec objet sp
library(sf) # nouveau package de classes et methodes spatiales doit "remplacer" rgdal et rgeos (et ofc sp) 
library(units) # gestion des unités pour ha
library(rmapshaper) #Visvalingam’s algorithm pour ms_simplify


#### lecture des communes en france source geofla 

commune.shp <- st_read("data/GEOFLA_COMMUNE_2016/COMMUNE_WGS84.shp")
summary(commune.shp)
str(commune.shp)

type_commune.dat <- read.csv("data/type_commune.csv", sep = "\t")
summary(type_commune.dat)
str(type_commune.dat)

commune_type.shp <- commune.shp %>% 
    left_join(type_commune.dat, by = c("INSEE_COM" = "CODGEO"))
    
table(commune_type.shp$STATUT_2016)

sum(table(commune_type.shp$STATUT_2016))
