#### Un script pour tous ce qui est commune, département, région et france métropolitaine
# je pense que c'est plus simple que de le reproduire dans des tas d'autres scripts
# pe penser à regarder si cela ne doit pas être un package

##.###################################################################################33
## I Chargement des différents packages demandés ====
##.#################################################################################33

source("code.R")

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

# pw <- {
#     chargelepwd # à charger avant
# }
# 
# # charge les drivers pour postgre 
# drv <- dbDriver("PostgreSQL")
# # class(drv) #une verif
# 
# # fais un pont vers la db réutilisable
# # ici j'ai pris une db en local pour tester
# # con sera utilisé pour chaque connection et pkoi le franciser
# con <- dbConnect(drv, dbname = "franceuser",
#                  host = "localhost", port = port, # attention 5432 par défaut
#                  user = "postgres", password = pw) # idem pour user
# rm(pw) # mouais


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
region.shp <- st_read(con,  query = "SELECT name, way
                                    FROM planet_osm_polygon
                                    WHERE boundary = 'administrative'  AND admin_level = '4';")


##    3 - Recup des departements ================
dpt.shp <- st_read(con,  query = "SELECT name, way
                                    FROM planet_osm_polygon
                                    WHERE boundary = 'administrative'  AND admin_level = '6';")


##.###################################################################################33
## III Traitements  ====
##.#################################################################################33

# un simplify, il faut js et la library mapshaper d'installer
# sys = TRUE l'utilise et evite de passer par une API
france_simplify.shp <- ms_simplify(region.shp, sys = TRUE) 

# plot(france_simplify.shp) verif

type_commune.dat <- read.csv("data/type_commune.csv", sep = "\t")
# summary(type_commune.dat)
# str(type_commune.dat)

# on utilise mapshaper il faut js mapshaper d'installer surtout ici avec sys= T 
commune_simplify.shp <- ms_simplify(commune_osm.shp, sys = TRUE) 

commune_type.shp <- commune_simplify.shp %>% 
    filter(!is.na(insee)) %>% # on fait un filtre avec les communes qui n'ont pas de COG des non fr
    left_join(type_commune.dat, by = c("insee" = "CODGEO")) # la jointure

france.shp <- commune_type.shp %>%
    filter(!is.na(insee)) %>% 
    st_union() # un gros merge sur l'ensemble

# st_crs(france.shp) # verif du CRS

#  un simplify sur les dpt

dpt_simplify.shp  <- ms_simplify(dpt.shp, sys = TRUE)

