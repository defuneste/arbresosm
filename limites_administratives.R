# Date: octobre 2018, cotobre 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: des limites administratives
# Description du problème:
# Un script pour tous ce qui est commune, département, région et france métropolitaine
# je pense que c'est plus simple que de le reproduire dans des tas d'autres scripts
# ils osnt sauver dans data pour gagner du temps
# Libraries utilisées: 
# "RPostgreSQL", "sf"

##.###################################################################################33
## I Chargement des différents packages demandés ====
##.#################################################################################33

source("OSM/connect_db_france.R")

library(sf) 
library(dplyr)
library(rmapshaper)

##.###################################################################################33
## II Import et mise en forme de différentes limites ====
##.#################################################################################33

##    1 - Recup des communes  ================
# on prend les communes dans OSM c'est plus à jour que geofla
commune_osm.shp  <- st_read(con,  query = "SELECT name,tags -> 'ref:INSEE' AS INSEE, way  
                                            FROM planet_osm_polygon
                                            WHERE boundary = 'administrative'  AND admin_level = '8';")

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
france_simplify.shp <- rmapshaper::ms_simplify(region.shp, sys = TRUE) 
# il y a des trucs pas fr

pas_en_france <- c("Solothurn", "Sardegna" , "Catalunya" , "Vaud" , "Genève")

france_simplify.shp <- france_simplify.shp[!grepl(paste(pas_en_france, 
                                collapse="|"),
                          france_simplify.shp$name), ]

dpt_simplify.shp <-  ms_simplify(dpt.shp , sys = TRUE) 

pas_en_france <- c("Gipuzkoa", "Girona",  "Bezirk Thierstein", "Nord-Est Sardegna")

dpt_simplify.shp <- dpt_simplify.shp[grepl(paste(pas_en_france, 
                                                       collapse="|"),
                                            dpt_simplify.shp$name), ]
# plot(france_simplify.shp) verif

type_commune.dat <- read.csv("data/type_commune.csv", sep = "\t")
# summary(type_commune.dat)
# str(type_commune.dat)

# on utilise mapshaper il faut js mapshaper d'installer surtout ici avec sys= T 
commune_simplify.shp <- ms_simplify(commune_osm.shp, sys = TRUE) 

commune_type.shp <- commune_simplify.shp %>% 
    dplyr::filter(!is.na(insee)) %>% # on fait un filtre avec les communes qui n'ont pas de COG des non fr
    dplyr::left_join(type_commune.dat, by = c("insee" = "CODGEO")) # la jointure

sf::st_write(commune_type.shp, "data/commune.geojson")
sf::st_write(france_simplify.shp, "data/regions_simp.geojson")
sf::st_write(dpt_simplify.shp, "data/dpt_simp.geojson")
