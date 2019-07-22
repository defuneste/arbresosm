#### Un script pour tous ce qui est commune, département, région et france métropolitaine
# je pense que c'est plus simple que de le reproduire dans des tas d'autres scripts
# pe penser à regarder si cela ne doit pas être un package

##.###################################################################################33
## I Chargement des différents packages demandés ====
##.#################################################################################33

### DB
library(RPostgreSQL) # fait le lien avec postgre, utilise DBI

### manip
library(dplyr) # manip de données en tidyverse
library(tibble)
library(tidyr)

### visualisation
library(ggplot2) # la visualisation
library(tmap) # carto
library(ggmap)# carto +
library(leaflet) # carto web

## analyse spatiale / carto
library(sp) # classes et methodes pour données spatiales pe déclassé par SF
library(rgdal) #gdal pour projection, crud et surtout export
library(rgeos) # geos, penser à installer libgeos++-dev avant, travail avec objet sp
library(sf) # nouveau package de classes et methodes spatiales doit "remplacer" rgdal et rgeos (et ofc sp) 
library(units) # gestion des unités pour ha
library(rmapshaper) #Visvalingam’s algorithm pour ms_simplify

# il faut établir une connexion 

pw <- {
    chargelepwd # à charger avant
}

# charge les drivers pour postgre 
drv <- dbDriver("PostgreSQL")
# class(drv) #une verif

# fais un pont vers la db réutilisable
# ici j'ai pris une db en local pour tester
# con sera utilisé pour chaque connection et pkoi le franciser
con <- dbConnect(drv, dbname = "franceuser",
                 host = "localhost", port = port, # attention 5432 par défaut
                 user = "postgres", password = pw) # idem pour user
rm(pw) # mouais


##.###################################################################################33
## II Import et mise en forme de différentes limites ====
##.#################################################################################33


##    1 - Recup des communes  ================
# on prend les communes dans OSM c'est plus à jour que geofla
commune_osm.shp  <- st_read(con,  query = "SELECT name,tags -> 'ref:INSEE' AS INSEE, way  
                                            FROM planet_osm_polygon
                                            WHERE boundary = 'administrative'  AND admin_level = '8';")
summary(commune_osm.shp) # petits verifs
st_crs(commune_osm.shp) # verification du CRS

##    2 - Recup des regions  ================
# on prend les regions dans OSM pour un fond
france.shp <- st_read(con,  query = "SELECT name, way
                                    FROM planet_osm_polygon
                                    WHERE boundary = 'administrative'  AND admin_level = '4';")

# un simplify, il faut js et la library mapshaper d'installer
# sys = TRUE l'utilise et evite de passer par une API
france_simplify.shp <- ms_simplify(france.shp, sys = TRUE) 
# plot(france_simplify.shp) verif

france.shp <- commune_type.shp %>%
    filter(!is.na(insee)) %>% 
    st_union() # un gros merge sur l'ensemble

st_crs(france.shp) # verif du CRS



