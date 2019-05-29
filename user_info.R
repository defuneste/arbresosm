### projet pour l'analyse des arbres seul dans OSM 
# exploration et "nettoyage des données"
# octobre 2018
# Petites notes sur le code :
# pour me rapeller qu'un objet à des infos geometriques type vecteur je lui accolle un .shp
# si c'est un raster .grid, si c'est un dataframe souvent un .dat

# chargement des différents packages demandés 

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

######## travail sur osm_user et sur l'encodage des arbres

# import des conributeurs et du moment de la contribution
user.shp <- dbGetQuery(con,  "SELECT  tags -> 'osm_timestamp' AS ts, tags -> 'osm_user' AS nom, tags -> 'osm_changeset' AS changeset,
                       osm_id AS ID, tags -> 'osm_uid' AS user_id, tags -> 'osm_version' AS version
                       FROM planet_osm_point
                       WHERE planet_osm_point.natural = 'tree';")
#verif de routine
dim(user.shp)
str(user.shp)

# # on prend les communes dans OSM c'est plus à jour que geofla
# commune_osm.shp  <- st_read(con,  query = "SELECT name,tags -> 'ref:INSEE' AS INSEE, way  
#                                             FROM planet_osm_polygon
#                                             WHERE boundary = 'administrative'  AND admin_level = '8';")
# summary(commune_osm.shp) # petits verifs
# st_crs(commune_osm.shp) # verification du CRS
# 
# # on utilise mapshaper il faut js mapshaper d'installer surtout ici avec sys= T 
# commune_simplify.shp <- ms_simplify(commune_osm.shp, sys = TRUE) 
# 
# commune_type.shp <- commune_simplify.shp %>% 
#     filter(!is.na(insee))
# 
# france.shp <- commune_type.shp %>%
#     st_union() # un gros merge sur l'ensemble
# st_crs(france.shp) # verif du CRS
# 
# user_france.shp <- species.shp[france.shp,] # je coupe avec la france
# 
# user.dat <- user_france.shp %>%
#     st_set_geometry(value = NULL) %>% #drop de la geometrie
# 
# user.dat$ts <- as_date(user.dat$ts) # on passe en POSIX juste date
# head(user.dat)

# j'ai pris la 15aine mais on est presque de l'ordre du jour
ggplot(user.dat, aes(x = ts)) +
    geom_histogram(binwidth = 15) +
    xlab("Années") +
    ylab("Nb. arbres isolés") +
    labs(caption ="source : © les contributeurs d’OpenStreetMap")

# une verif pour s'assurer que osm_uid correspond bien à l'id de l'user
user.dat %>% 
    filter(nom == "defuneste")

# liste des utilisteurs avec le nombre d'arbres
liste_user.dat <- user.dat %>% 
    #filter(ts > "2018-11-01") %>% 
    group_by(nom) %>% 
    summarise(nb_arbre = n(),
              duree = max(ts) - min(ts)) %>% 
    arrange(desc(nb_arbre)) 

#  nombre d'utilsateurs contribuant aux arbres
dim(liste_user.dat)

## comptage du nombre de jour ou un arbre à été ajouté modifié

nb_jour <- user.dat %>% 
    #filter(ts > "2018-11-01") %>% 
    group_by(nom, ts) %>% 
    summarise(tot = 1,
              arbre_date = n()) %>%
    group_by(nom) %>% 
    summarise(nb_jour =  sum(tot),
              max_arbre_jour = max(arbre_date))

# une petite verification avec une user

user.dat %>% 
    filter(nom == "_phiphou_") %>%
    distinct(ts)

# join attention sur le meme nom
liste_user.dat <- liste_user.dat %>%
    left_join(nb_jour, by = "nom")

dim(liste_user.dat)

## constitution d'un tableau pour les profils d'utilisteurs : 

# nb de polygones
poly_count.dat <- dbGetQuery(con, "SELECT tags -> 'osm_user' AS nom, COUNT (*) AS poly_count
                             FROM planet_osm_polygon
                             WHERE  tags -> 'osm_user' IN (SELECT DISTINCT tags -> 'osm_user' AS nom
                             FROM planet_osm_point
                             WHERE planet_osm_point.natural = 'tree')
                             GROUP BY nom
                             ORDER BY poly_count DESC;")
dim(poly_count.dat)

#nb de lignes
ligne_count.dat <- dbGetQuery(con, "SELECT tags -> 'osm_user' AS nom, COUNT (*) AS ligne_count
                              FROM planet_osm_roads
                              WHERE  tags -> 'osm_user' IN (SELECT DISTINCT tags -> 'osm_user' AS nom
                              FROM planet_osm_point
                              WHERE planet_osm_point.natural = 'tree')
                              GROUP BY nom
                              ORDER BY ligne_count DESC;" )
dim(ligne_count.dat)

# nb de poi
poi_count.dat <- dbGetQuery(con, "SELECT tags -> 'osm_user' AS nom, COUNT (*) AS poi_count
                            FROM planet_osm_point
                            WHERE  tags -> 'osm_user' IN (SELECT DISTINCT tags -> 'osm_user' AS nom
                            FROM planet_osm_point
                            WHERE planet_osm_point.natural = 'tree')
                            GROUP BY nom
                            ORDER BY poi_count DESC;")
dim(poi_count.dat)

# join et passage des na en 0 , on fait aussi une typo en fonction du1/9/90

profil_user.dat <- liste_user.dat %>%
    left_join(poi_count.dat, by = "nom") %>%
    left_join(ligne_count.dat, by = "nom") %>% 
    left_join(poly_count.dat , by = "nom") %>% 
    replace_na(list(ligne_count = 0, poly_count = 0)) %>%
    mutate(ranking = 1:length(liste_user.dat$nom),
           type_user = factor(case_when(ranking <= round(length(liste_user.dat$nom)/100) ~ "1%", 
                                 ranking > round(length(liste_user.dat$nom)/100)  &
                                 ranking <= round((length(liste_user.dat$nom)/100)*10) ~ "9%",
                                 ranking > round((length(liste_user.dat$nom)/100)*10) ~ "90%")
                              , levels = c("90%","9%","1%"), ordered = T))

## une petite verif
table(profil_user.dat$type_user)

# graph des sum des 90/9/1
ggplot(profil_user.dat, aes(y = nb_arbre, x = type_user)) +
    geom_bar(stat = "summary_bin", fun.y = "sum") +
    scale_x_discrete("Contributeurs") +
    scale_y_continuous("Nombres d'arbres")


tail(profil_user.dat)

plot(profil_user.dat$nb_arbre, profil_user2.dat$arbre_jour)

summary(profil_user.dat)

profil_user.dat$type_user

# sauver si besoin
write.csv(profil_user.dat2, "profile.csv")

# des greps pour tester des pseudo de com com
# unique(user.dat$user)[grep(pattern = "Paris|paris", unique(user.dat$user))]

# se deconnecter de la base

dbDisconnect(con)