### projet pour l'analyse des arbres seul dans OSM 
# exploration et "nettoyage des données"
# octobre 2020
# Petites notes sur le code :
# pour me rapeller qu'un objet à des infos geometriques type vecteur je lui accolle un .shp
# si c'est un raster .grid, si c'est un dataframe souvent un .dat

##.###################################################################################33
## I Chargement des différents packages demandés et données ====
##.#################################################################################33

# chargement des codes depend de l'envt  
source("code.R")

pkgs <-  c("RPostgreSQL","dplyr", "lubridate", "ggplot2", "sf")
inst <- lapply(pkgs, library, character.only = TRUE)

# il faut établir une connexion 

# charge les drivers pour postgre 
drv <- dbDriver("PostgreSQL")
# class(drv) #une verif

# fais un pont vers la db réutilisable
# ici j'ai pris une db en local pour tester
# con sera utilisé pour chaque connection et pkoi le franciser
con <- dbConnect(drv, dbname = dbname,
                 host = "localhost", port = port, 
                 user = "postgres", password = chargelepwd) # idem pour user

rm(chargelepwd, port, dbname)

dbExistsTable(con, "points") # une verification

##.###################################################################################33
## II debut explo ====
##.#################################################################################33

# soyons fou 

user.shp <- st_read(con,  query = "select osm_uid, osm_timestamp from points;")

user.shp$osm_timestamp <- as.Date(user.shp$osm_timestamp)
# on peut aussi regarder les heures de cartographie mais attention au decalage horaires

ggplot(user.shp, aes(x = osm_timestamp)) +
    geom_histogram(binwidth = 15) +
    xlab("Années") +
    ylab("Nb. arbres isolés") +
    labs(caption ="source : © les contributeurs d’OpenStreetMap")