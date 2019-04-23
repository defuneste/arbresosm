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


#### lecture des communes en france source geofla =======

####  regarder avec OSM
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

# on prend les communes dans OSM c'est plus à jour que geofla

commune_osm.shp  <- st_read(con,  query = "SELECT name,tags -> 'ref:INSEE' AS INSEE, way  
FROM planet_osm_polygon
WHERE boundary = 'administrative'  AND admin_level = '8';")
summary(commune_osm.shp) # petits verifs
st_crs(commune_osm.shp)

# on utilise mapshaper il faut js mapshaper d'installer surtout ici avec sys= T 
commune_simplify.shp <- ms_simplify(commune_osm.shp, sys = TRUE) 

# une fois le simplify ok on libère de la place
rm(commune_osm.shp)

#commune.shp <- st_read("data/GEOFLA_COMMUNE_2016/COMMUNE_WGS84.shp") # je prend pas le geofla 
#summary(commune.shp)                                                 # au final OSM est mieux
#str(commune.shp)                                                     # regarder admin express

#####
type_commune.dat <- read.csv("data/type_commune.csv", sep = "\t")
summary(type_commune.dat)
str(type_commune.dat)

commune_type.shp <- commune_simplify.shp %>% 
    filter(!is.na(insee)) %>% 
    left_join(type_commune.dat, by = c("insee" = "CODGEO"))

# verification des NA
commune_type.shp %>%
    st_set_geometry(value = NULL) %>%  # on drop la geometrie pour du gain de tps
    group_by(STATUT_2016) %>% 
    summarise(comptage = n())

type_commune.dat %>% # il y a pas de NA dans type commune 
    filter(is.na(STATUT_2016))

table(commune_type.shp$STATUT_2016)

# st_write(commune_type.shp, "commune_type.geojson")



