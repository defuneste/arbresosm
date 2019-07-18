### projet pour l'analyse des arbres seul dans OSM 
# exploration et "nettoyage des données"
# octobre 2018
# Petites notes sur le code :
# pour me rapeller qu'un objet à des infos geometriques type vecteur je lui accolle un .shp
# si c'est un raster .grid, si c'est un dataframe souvent un .dat

##.###################################################################################33
## I Chargement des différents packages demandés ====
##.#################################################################################33

#  

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


##.###################################################################################33
## II Ouverture et preprocessing des données ====
##.#################################################################################33

##    1 - Recup des Arbres OSM  ================

# import des conributeurs et du moment de la contribution
user.shp <- st_read(con,  query = "SELECT  way, tags -> 'osm_timestamp' AS ts, tags -> 'osm_user' AS nom, tags -> 'osm_changeset' AS changeset,
                       osm_id AS ID, tags -> 'osm_uid' AS user_id, tags -> 'osm_version' AS version
                       FROM planet_osm_point
                       WHERE planet_osm_point.natural = 'tree';")
#verif de routine
dim(user.shp)
str(user.shp)

##    2 - Recup des shape util  ================
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
# sys = TRUE l'utilise et evite de passer par une API
france_simplify.shp <- ms_simplify(france.shp, sys = TRUE) 
# plot(france_simplify.shp) verif

france.shp <- commune_type.shp %>%
    filter(!is.na(insee)) %>% 
    st_union() # un gros merge sur l'ensemble

st_crs(france.shp) # verif du CRS

##    3 - Preprocessing des données  ================

user_france.shp <- user.shp[france.shp,] # je coupe avec la france

user.dat <- user_france.shp %>%
    st_set_geometry(value = NULL)  #drop de la geometrie

user.dat$ts <- as_date(user.dat$ts) # on passe en POSIX juste date
head(user.dat) # une verif

##.###################################################################################33
## III Travail sur osm_user et sur l'encodage des arbress ====
##.#################################################################################33


##    1 - Evolution dans le temps des arbres dans OSM   ================

# j'ai pris la 15aine mais on est presque de l'ordre du jour
# ici sur un fichier léger

ggplot(user.dat, aes(x = ts)) +
    geom_histogram(binwidth = 15) +
    xlab("Années") +
    ylab("Nb. arbres isolés") +
    labs(caption ="source : © les contributeurs d’OpenStreetMap")

# si on veut aller un peu plus loin et verifier d'ou vient la donnée on peut utilsier les champs refs
# il faut prendre cette base dans exploration, je devrais pe en faire une version r binary pour gagner 
# du temps

# un grep pour avoir les noms de variables qui commence par ref
import_bd <- names(arbres_osm)[grep(pattern = "^ref", names(arbres_osm))]

# ref est etrange et doit être un peu osculté, il semble cependant bien que ce soit une ref d'une base

pas_na <- function(x) {sum(!is.na(x))} # une fonction qui somme les T/F sur des pas Na

# ici je compte les nom Na pour les refs
lapply(arbres_osm[names(arbres_osm)[grep(pattern = "^ref", names(arbres_osm))]], pas_na)

arbres_osm$refbd <- apply(arbres_osm[,import_bd], 1, pas_na)

table(arbres_osm$refbd)  # j'ai des cheffauchement au max 3 et souvent 2 sur Paris

# ici je prefére du 0/1 mais c'est discutable
arbres_osm$refbd[arbres_osm$refbd >= 1] <- 1

# une petite verif visuelle : il y a pb des erreurs avec "ref"
# arbres_osm %>% 
#     filter(refbd == 1) %>% 
#     select(import_bd, refbd) %>% 
#     view()

# on passe dans un format dates
arbres_osm$osm_timestamp <- as.Date(arbres_osm$osm_timestamp) # en date attention on perd l'heure/minutes

ggplot(arbres_osm, aes(x = osm_timestamp, fill = as.factor(refbd))) +
    geom_histogram(binwidth = 15) +
    xlab("Year") +
    ylab("Isolated trees") +
    labs(caption ="© OpenStreetMap contributors") + 
    guides(fill=guide_legend(title="Ref. from DB import:")) +
    scale_fill_manual(labels = c("- Not indicated", "- Indicated"), values = c("#1b9e77", "#7570b3"))


## ici une version de ce graph en tenant compte de l'info botanique
arbres_osm$info <- 0 # un nouveau champ
arbres_osm$info[!is.na(arbres_osm$genus)] <- 1 # il prend 1 quamd j'ai une info sur genus
arbres_osm$info[!is.na(arbres_osm$species)] <- 1 # il prend 1 quamd j'ai une info sur species


ggplot(arbres_osm, aes(x = osm_timestamp, fill = as.factor(info))) +
    geom_histogram(binwidth = 15) +
    xlab("Year") +
    ylab("Isolated trees") +
    labs(caption ="© OpenStreetMap contributors") +
    guides(fill=guide_legend(title="Botanic information:")) +
    scale_fill_manual(labels = c("- not indicated", "- indicated"), values = c("#d95f02", "#7570b3"))

# une verif pour s'assurer que osm_uid correspond bien à l'id de l'user
user.dat %>% 
    filter(nom == "defuneste")

##    2 - Contribution cumulée de chaques utilisateurs    ================

# liste des utilisteurs avec le nombre d'arbres
liste_user.dat <- user.dat %>% 
    #filter(ts > "2018-11-01") %>% 
    group_by(nom) %>% 
    summarise(nb_arbre = n(),
              duree = max(ts) - min(ts)) %>% 
    arrange(nb_arbre) %>% # ici je classe par par ordre descendant car je vais utiliser plus loin pour faire une courbe de lorentz
    mutate(order = 1:length(liste_user.dat$nom)) 

#  nombre d'utilsateurs contribuant aux arbres
dim(liste_user.dat)

median(liste_user.dat$nb_arbre) # mediane
mean(liste_user.dat$nb_arbre) # moyenne

liste_user.dat$cumsum_nbarbre <- cumsum(liste_user.dat$nb_arbre) # somme cumulée des arbres

# ici order est divisé par son max et pareil pour la somme cumuléé
# cela les passe de 0 à 1 et on modifie les labels avec 
# scales::percent
ggplot(liste_user.dat, aes(x = order/max(liste_user.dat$order), y = cumsum_nbarbre/max(liste_user.dat$cumsum_nbarbre))) +
    geom_line(col = "forestgreen", lwd = 1.5) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs( x = "Pourcentage d'arbres ajoutés",
          y = "Pourcentage de contributeurs", 
          caption = "© OpenStreetMap contributors") + 
    theme_bw()

plot(liste_user.dat$cumsum_nbarbre, liste_user.dat$nb_arbre)

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