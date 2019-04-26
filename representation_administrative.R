### projet pour l'analyse des arbres seul dans OSM 
# exploration et "nettoyage des données"
# Petites notes sur le code :
# pour me rapeller qu'un objet à des infos geometriques type vecteur je lui accolle un .shp
# si c'est un raster .grid, si c'est un dataframe souvent un .dat

# Chargement des différents packages demandés ===========

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
library(tmaptools) # + outils pour tmap
library(ggmap)# carto +
library(leaflet) # carto web
library(rsconnect) # pour partager une carte
library(RColorBrewer) # des palettes

## analyse spatiale / carto
library(sp) # classes et methodes pour données spatiales pe déclassé par SF
library(rgdal) #gdal pour projection, crud et surtout export
library(rgeos) # geos, penser à installer libgeos++-dev avant, travail avec objet sp
library(sf) # nouveau package de classes et methodes spatiales doit "remplacer" rgdal et rgeos (et ofc sp) 
library(units) # gestion des unités pour ha
library(rmapshaper) #Visvalingam’s algorithm pour ms_simplify


#### Lecture des communes en france source geofla =======

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
st_crs(commune_osm.shp) # verification du CRS

# on prend les regions dans OSM pour un fond
france.shp <- st_read(con,  query = "SELECT name, way
FROM planet_osm_polygon
WHERE boundary = 'administrative'  AND admin_level = '4';")

# un simplify, il faut js et la library mapshaper d'installer
# sys = TRUE l'utilise
france_simplify.shp <- ms_simplify(france.shp, sys = TRUE) 
# plot(france_simplify.shp) verif

# on prends les arbres
species.shp <- st_read(con,  query = "SELECT way, tags -> 'species' AS species, tags -> 'genus' AS genus
FROM planet_osm_point
WHERE planet_osm_point.natural = 'tree';")

# on utilise mapshaper il faut js mapshaper d'installer surtout ici avec sys= T 
commune_simplify.shp <- ms_simplify(commune_osm.shp, sys = TRUE) 

# une fois le simplify ok on libère de la place
rm(commune_osm.shp)

#commune.shp <- st_read("data/GEOFLA_COMMUNE_2016/COMMUNE_WGS84.shp") # je prend pas le geofla 
#summary(commune.shp)                                                 # au final OSM est mieux
#str(commune.shp)                                                     # regarder admin express

##### jointure avec type de communes ===================
type_commune.dat <- read.csv("data/type_commune.csv", sep = "\t")
summary(type_commune.dat)
str(type_commune.dat)

commune_type.shp <- commune_simplify.shp %>% 
    filter(!is.na(insee)) %>% # on fait un filtre avec les communes qui n'ont pas de COG des non fr
    left_join(type_commune.dat, by = c("insee" = "CODGEO")) # la jointure

# verification des NA
commune_type.shp %>%
    st_set_geometry(value = NULL) %>%  # on drop la geometrie pour du gain de tps
    group_by() %>% 
    summarise(comptage = n())

type_commune.dat %>% # il y a pas de NA dans type commune 
    filter(is.na(STATUT_2016))

table(commune_type.shp$STATUT_2016) # sur les statuts

# ici un export en json attemtion si on veut du shape c'est du ISO qui aime pas les accents
#st_write(commune_type.shp, "commune_type.geojson") 

# cartographie de vérification ici c'est juste un carte des commune rurale/urbaine

france_commune_map <- tm_shape(commune_type.shp)  # un objet tmap "shape"

france_commune_map + #  on ajoute à shape
            tm_borders(alpha = 0.2) + # des bordures moins transparentes
            # on remplie les polygones 
            tm_fill(col="TYPE_COM", labels = c("Rural", "Urbain"), title = "Types communes") +
            # les credits
            tm_credits("Source : © les contributeurs d’OpenStreetMap", size = 0.4, position=c("left", "top")) +
            # une bar
            tm_scale_bar(position = c( "center", "BOTTOM"))


### stats par type de commune ==============

# un polygone pour la france provenant des données admnistrative
france.shp <- commune_type.shp %>%
        st_union() # un gros merge sur l'ensemble
st_crs(france.shp) # verif du CRS

# on ne garde que les arbres dans ces limites

str(species.shp) # on regarde combien on a d'arbres

species_france.shp <- species.shp[france.shp,] # on coupe pour la france
str(species_france.shp) # on perd pas mal d'arbres, geneve ? 

# jointure des types de communes à la table des arbres
species_france_type.shp <- st_join(species_france.shp, commune_type.shp[c("TYPE_COM", "insee")])
# besoin des arbres mais aussi des arbres avec un peu d'info
# je vais definir le "un peu d'info" comme ayant une indication au niveau du genre ou de l'espece meme fausse
# il y a peu d'erreurs sur le genre donc c'est moins grave, espèce est plus compliquée

species_france_type.shp$info <- 0 # un nouveau champ
species_france_type.shp$info[!is.na(species_france_type.shp$genus)] <- 1 # il prend 1 quamd j'ai une info sur genus
species_france_type.shp$info[!is.na(species_france_type.shp$species)] <- 1 # il prend 1 quamd j'ai une info sur species
sum(species_france_type.shp$info)

# nb d'arbres par types de commune
table(species_france_type.shp$TYPE_COM)
# RURAL URBAIN 
# 125510 694969 
# nb d'arbres avec info par types de commune
table(species_france_type.shp$TYPE_COM[species_france_type.shp$info == 1])
# RURAL URBAIN 
# 1620 162671 

# calcul de la superficie par commune en km2, utilisation d'units 
commune_type.shp$surface_km2 <-  set_units(
    # on calcul la surface
    st_area(
        # il faut avant passer en EPSG 4326 sinon il confond degre et m
        st_transform(commune_type.shp, 4326)), 
    value = km2) # ici on aurait pu mettre ha cf la doc d'units 

# calcul de la surface par type de commune
commune_type.shp %>%
    st_set_geometry(value = NULL) %>% #drop de la geometrie
    group_by(TYPE_COM) %>% 
    summarise(surface = sum(surface_km2)) # une somme sur les surfaces


# cartes par communes de la presence d'arbres ================

# un intermediaire pour avoir l nombre d'arbres present par COG
arbre_commune <- species_france_type.shp %>%                # on part des arbres
    st_set_geometry(value = NULL) %>%                       # on enleve la geometrie
    group_by(insee) %>%                                     # on groupe par COG
    summarise(nbr_arbre = n(),                              # on compte le nombre d'arbre par COG
              nbr_arbre_info = sum(info)) %>%               # com compte le mombre d'arbre avec info par COG
    mutate(tx_info = (nbr_arbre_info/nbr_arbre) *100 )      # calcul d'un taux de renseignement/info par COG    

# hist(arbre_commune$tx_info) 3 deux modes 

# une jointure sur le shape des commune par le COG
commune_type.shp <- commune_type.shp %>% 
    left_join(arbre_commune, by = c("insee" = "insee"))

# calcul de nouveaux paramètres
# en dessous remplace les NA par 0, je prefere aucune donnée à 0 mais cela se discute
# commune_type.shp$nbr_arbre[is.na(commune_type.shp$nbr_arbre)] <- 0 
# calcul de densité 
commune_type.shp$densite_arbre <- commune_type.shp$nbr_arbre/commune_type.shp$surface_km2
commune_type.shp$densite_arbre_info <- commune_type.shp$nbr_arbre_info/commune_type.shp$surface_km2
# un pourcentage de renseignement sur les arbres present 


#drop des units pour le case_when qui suit, sinon il demande que mes autres valeurs
# 0, 0.2 , 1 soit en units aussi 
commune_type.shp$densite_arbre_info <- drop_units(commune_type.shp$densite_arbre_info)
commune_type.shp$densite_arbre <- drop_units(commune_type.shp$densite_arbre)
summary(commune_type.shp) # une verif

commune_type.shp <- commune_type.shp %>% # creation d'un nouveau fichier
    mutate(class_densité = case_when( # on reclassifie la densite
              #densite_arbre == 0 ~ "0", # ici si on utilise 0 plutot que NA
               densite_arbre > 0 & densite_arbre <= 0.2 ~ "1",
               densite_arbre > 0.2 & densite_arbre <= 1 ~ "2",
               densite_arbre > 1 & densite_arbre <= 10 ~ "3",
               densite_arbre > 10 & densite_arbre <= 100 ~ "4",
               densite_arbre > 100 & densite_arbre <= 500 ~ "5",
               densite_arbre > 500 & densite_arbre <= 1000 ~ "6",
               densite_arbre > 1000 & densite_arbre <= 1500 ~ "7",
               densite_arbre > 1500 ~ "8"),
            class_densité_info = case_when( # on reclassifie la densite
               #densite_arbre == 0 ~ "0", # ici si on utilise 0 plutot que NA
               densite_arbre_info > 0 & densite_arbre_info <= 0.2 ~ "1",
               densite_arbre_info > 0.2 & densite_arbre_info <= 1 ~ "2",
               densite_arbre_info > 1 & densite_arbre_info <= 10 ~ "3",
               densite_arbre_info > 10 & densite_arbre_info <= 100 ~ "4",
               densite_arbre_info > 100 & densite_arbre_info <= 500 ~ "5",
               densite_arbre_info > 500 & densite_arbre_info <= 1000 ~ "6",
               densite_arbre_info > 1000 & densite_arbre_info <= 1500 ~ "7",
               densite_arbre_info > 1500 ~ "8")
           )

table(commune_type.shp$class_densité_info) # un table pour verifier sur les classes de densités par commune 

france_commune_map <- tm_shape(commune_type.shp) # on sauve le shape dans un objet tmap

# la legende, peut aussi être mis dans le case_when
legende <- c("]0-0,2]", "]0,2-1]", "]1-10]", "]10-100]", "]100-500]", "]500-1000]", "]1000-1500]", "]1500+" )

# une carte soit des arbres avec ou sans info
france_commune_map +
    tm_fill(col = "class_densité_info", palette = "Oranges", n = 8, contrast = c(0, 1), # remplie les polygones avec greens 
                                                                                  # ici on peut prendre class_densité et 
                                                                                  # class_densité_info
            title = "Arbres isolés par km²", labels = legende,                        # titre de la legende, label prend legend
            textNA = "Aucune donnée",                                             # le label des NA
            colorNA = "white") +                                                  # les Na en blanc
    tm_shape(france_simplify.shp) +                                               # rajout d'un shape pour les regions
    tm_borders(col = "grey") +                                                    # juste les bordures en gris
    tm_credits("Source : © les contributeurs d’OpenStreetMap", size = 0.5, position=c("left", "top")) + # sources
    tm_scale_bar(position = c( "center", "BOTTOM")) +                             # legende ici juste sa position
    tm_legend(title = "Arbres isolés renseignés (genre ou espèce)/km² par commune")                            # titre

# une carte avec le taux

# une carte soit des arbres avec ou sans info
france_commune_map +
    tm_fill(col = "tx_info", palette = "Oranges", n = 10, contrast = c(0, 1), # remplie les polygones avec greens 
            # ici on peut prendre class_densité et 
            # class_densité_info
            title = "Pourcentage d'arbres renseignés²",                       # titre de la legende, label prend legend
            textNA = "Aucune donnée",                                             # le label des NA
            colorNA = "white") +                                                  # les Na en blanc
    tm_shape(france_simplify.shp) +                                               # rajout d'un shape pour les regions
    tm_borders(col = "grey") +                                                    # juste les bordures en gris
    tm_credits("Source : © les contributeurs d’OpenStreetMap", size = 0.5, position=c("left", "top")) + # sources
    tm_scale_bar(position = c( "center", "BOTTOM")) +                             # legende ici juste sa position
    tm_legend(title = "Pourcentage d'arbres renseignés par commune")   
