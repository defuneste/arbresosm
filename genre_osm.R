# un script pour le genre =======
### projet pour l'analyse des arbres seul dans OSM 
# Petites notes sur le code :
# pour me rapeller qu'un objet à des infos geometriques type vecteur je lui accolle un .shp
# si c'est un raster .grid, si c'est un dataframe souvent un .dat

# chargement des différents packages demandés ======

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

# vérifie pour une table 
dbExistsTable(con, "planet_osm_point") 

##### les genres dans OSM france 

# on va commencer par dropper la geometrie pour une première analyse et aller plus vite 

species.shp <- st_read(con,  query = "SELECT way, tags -> 'species' AS species, tags -> 'genus' AS genus
FROM planet_osm_point
WHERE planet_osm_point.natural = 'tree';")

class(species.shp) # on vérifie la classe


species.dat <- st_set_geometry(species.shp, value = NULL) # on drop la geometrie
class(species.dat) # un df

sort(table(species.dat$genus), decreasing =  T)

length(sort(table(species.dat$genus), decreasing =  T)) # 265 genres avant nettoyage

sum(table(species.dat$genus)) # 80170 renseignements dans "genus"

# des greps pour verifier ====
unique(species.dat$genus)[grep(pattern = "^[[:lower:]]", unique(species.dat$genus))] # genres commencant par une minuscule
unique(species.dat$genus)[grep(pattern = "\\s", unique(species.dat$genus))] # genres contenant un espace
unique(species.dat$genus)[grep(pattern = "\\;", unique(species.dat$genus))] # genres contenant un ;
unique(species.dat$genus)[grep(pattern = "(\\è|é|ê)", unique(species.dat$genus))]  # genres contenant un è, é, ê

# Une serie de fonctions pour regarder les erreurs ==============

#la fonction prend une regex et une collone de verification par défaut "genus"

tableau_selection <- function(patron_regex, colonne="genus") {
    filtre <- unique(species.dat$genus)[grep(pattern = patron_regex, unique(species.dat[[colonne]]))]  
    decompte <- species.dat %>%
        filter(genus %in% filtre) %>%
        group_by(genus) %>%
        summarize(comptage = n()) %>%
        arrange(desc(comptage))
    # impression du tibble
    print(decompte, n = Inf)
    # impression du nombre de ligne concernées
    # sum(decompte$comptage)
}


# on regarde les accents 

tableau_selection("(\\è|é|ê)")

# on capitalise la premiere lettre et on ne garde pas ceux absent dans les nom capitalisé

tableau_selection_capital <- function(patron_regex, negation = FALSE) {
    
    genus_upper <- str_to_title(unique(species.dat$genus)[grep(pattern = patron_regex, unique(species.dat$genus))])
    
    if(negation == TRUE){
        genre <- genus_upper[genus_upper %in% unique(species.dat$genus)]
        decompte <- species.dat %>%
            filter(genus %in% str_to_lower(genre)) %>%
            group_by(genus) %>%
            summarize(comptage = n()) %>%
            arrange(desc(comptage)) }
    else {
        genre <- genus_upper[!genus_upper %in% unique(species.dat$genus)]
        decompte <- species.dat %>%
            filter(genus %in% str_to_lower(genre)) %>%
            group_by(genus) %>%
            summarize(comptage = n()) %>%
            arrange(desc(comptage)) }
    print(decompte)
    sum(decompte$comptage)
}

tableau_selection_capital("^[[:lower:]]", negation = T)
tableau_selection_capital("^[[:lower:]]", negation = F)

# genre comprenant un espace et ayant donc plusieurs mots

tableau_selection_essence <- function(patron_regex, negation = FALSE) {
    
    genre <- unique(species.dat$genus)[grep(pattern = patron_regex, unique(species.dat$genus))]
    
    if(negation == TRUE){
        genre <- genre[genre %in% unique(species.dat$species)]
        decompte <- species.dat %>%
            filter(genus %in% genre) %>%
            group_by(genus) %>%
            summarize(comptage = n()) %>%
            arrange(desc(comptage)) }
    else {
        genre <- genre[!genre %in% unique(species.dat$species)]
        decompte <- species.dat %>%
            filter(genus %in% genre) %>%
            group_by(genus) %>%
            summarize(comptage = n()) %>%
            arrange(desc(comptage)) }
    print(decompte)
    sum(decompte$comptage)
}

tableau_selection_essence("\\s", negation = T)
tableau_selection_essence("\\s", negation = F)


# les ;

tableau_selection("\\;")

# on regarde où sont les emplacement libre =========
emplacement_libre <- species.shp %>%
    filter(genus == "Emplacement libre")

emplacement_libre <- st_transform(emplacement_libre, 4326)
st_crs(emplacement_libre)

st_write(emplacement_libre, dsn = "emplacement_libre.gpx", layer = "waypoints", driver = "GPX", 
         dataset_options = "GPX_USE_EXTENSIONS=yes")

carto_lyon <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = emplacement_libre, radius = 2, 
                     color = "green")


# si on selectionne toutes les erreurs et on regarde les non marqués comme erreurs dans les liste de genre d'osm

#travail maison 
# species.dat<- read.csv("species.dat")
# summary(species.dat)


erreurs <- tableau_selection("\\;|è|é|ê|^[[:lower:]]|\\s")

genre_corecte <- as.character(sort(unique(species.dat$genus)[!unique(species.dat$genus) %in% erreurs$genus]))

liste_genre <- read.csv("leaft_type.csv", sep = "\t", stringsAsFactors = F)
summary(liste_genre)
liste_genre2 <- read.csv("leaf_type_ajout.csv", sep = "\t", stringsAsFactors = F)
summary(liste_genre2)


genre_osm <- sort(c(liste_genre$Genus, liste_genre2$Genus))

genre_corecte[!genre_corecte %in% genre_osm]

## on va comparer à la list produite en verifiant les noms de genre 

liste_genre_metro <- read.csv("data/liste_genre_metro.csv", sep = "\t", stringsAsFactors = FALSE, strip.white=TRUE)
summary(liste_genre_metro$genus..)

species.dat %>%
    filter(genus %in% liste_genre_metro$genus..) %>%
    group_by(genus) %>%
    summarize(comptage = n()) %>%
    arrange(desc(comptage)) %>%
    filter(!is.na(genus)) %>% # on enleve les valeurs manquantes
    filter(comptage >=100) %>% 
    ggplot(aes(y= comptage, x = reorder(genus, comptage))) +
    geom_bar(stat = "identity") + 
    labs(x="Genre",
         ylab="Nombre d'arbres",
         caption ="source : © les contributeurs d’OpenStreetMap") +
    coord_flip() 
    
