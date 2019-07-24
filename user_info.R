### projet pour l'analyse des arbres seul dans OSM 
# exploration et "nettoyage des données"
# octobre 2018
# Petites notes sur le code :
# pour me rapeller qu'un objet à des infos geometriques type vecteur je lui accolle un .shp
# si c'est un raster .grid, si c'est un dataframe souvent un .dat

##.###################################################################################33
## I Chargement des différents packages demandés ====
##.#################################################################################33

# chargement des codes depend de l'ent  
source("code.R")

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

# chargement du scripts des limites administratives
source("limites_administratives.R")
rm(commune_osm.shp) # on enleve ce fichier gros et pas utile pour de la visualisation

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
    group_by(nom) %>% 
    summarise(nb_arbre = n(),
              duree = max(ts) - min(ts)) %>% 
    arrange(nb_arbre) # ici je classe par par ordre descendant car je vais utiliser plus loin pour faire une courbe de lorentz
liste_user.dat <- liste_user.dat %>% 
 mutate(order = 1:length(liste_user.dat$nom)) 


#  nombre d'utilsateurs contribuant aux arbres
dim(liste_user.dat)

median(liste_user.dat$nb_arbre) # mediane
mean(liste_user.dat$nb_arbre) # moyenne

liste_user.dat$cumsum_nbarbre <- cumsum(liste_user.dat$nb_arbre) # somme cumulée des arbres
sum(liste_user.dat$nb_arbre) == max(liste_user.dat$cumsum_nbarbre) # verif de parano

# des pourcentages
liste_user.dat <- liste_user.dat %>% 
    mutate(pourcent_order = order/max(liste_user.dat$order)*100,
           pourcent_arbre = cumsum_nbarbre/max(liste_user.dat$cumsum_nbarbre)*100)

# ici j' ai regarde les rangs
text_lorentz <- slice(liste_user.dat, c(2495,2549,2564))
label_text_lorentz <- c("73 contributeurs", "20 contributeurs", "5 contributeurs")


## constitution d'un tableau pour les profils d'utilisteurs : 

dbListTables(con)

# on produit un tableau avec le decompte par types
# attention je part du principe que la zone (france) de la base de données
# est bien correspondante : ce qui est pas tout à le cas

poly_count.dat <- dbGetQuery(con, "SELECT tags -> 'osm_user' AS nom, COUNT (*) AS count
                             FROM planet_osm_polygon 
                             GROUP BY nom;")

ligne_count.dat <- dbGetQuery(con, "SELECT tags -> 'osm_user' AS nom, COUNT (*) AS count
                              FROM planet_osm_roads
                              GROUP BY nom;" )

poi_count.dat <- dbGetQuery(con, "SELECT tags -> 'osm_user' AS nom, COUNT (*) AS count
                            FROM planet_osm_point
                            GROUP BY nom;")
# on ajoute le tout

OSM_user <- rbind(poly_count.dat, ligne_count.dat, poi_count.dat )

OSM_user_tot <- OSM_user %>% 
    group_by(nom) %>% 
    summarise(comptage = sum(count)) %>% 
    arrange(comptage) # ici je classe par par ordre descendant car je vais utiliser plus loin pour faire une courbe de lorentz

OSM_user_tot <- OSM_user_tot %>% 
    mutate(order = 1:length(OSM_user_tot$nom)) 

OSM_user_tot$cumsum_nbarbre <- cumsum(OSM_user_tot$comptage) # somme cumulée des arbres

sum(OSM_user_tot$comptage) # nombre d'objet dans OSM france


# ici order est divisé par son max et pareil pour la somme cumuléé
# cela les passe de 0 à 1 et on modifie les labels avec 
# scales::percent
ggplot(liste_user.dat, aes(x = order/max(liste_user.dat$order), y = cumsum_nbarbre/max(liste_user.dat$cumsum_nbarbre))) +
    geom_path(aes(colour = "forestgreen"), size = 1.5) +
        geom_line(data = OSM_user_tot, aes(x = order/max(OSM_user_tot$order) , 
                                       y = cumsum_nbarbre/max(OSM_user_tot$cumsum_nbarbre), colour = "gray30"), lty = 2) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs( y = "Pourcentage d'arbres ajoutés",
          x = "Pourcentage de contributeurs", 
          caption = "© Contributeurs OpenStreetMap ") + 
    geom_text(data = text_lorentz, aes(x = order/max(liste_user.dat$order), y = cumsum_nbarbre/max(liste_user.dat$cumsum_nbarbre)),
              label = label_text_lorentz, hjust= 1.1, col = "gray50") +
    scale_colour_manual(name = '', 
                        values =c("forestgreen"="forestgreen","gray30"="gray30"), labels = c("Arbres isolés","Objets OSM")) +
    theme_bw() +
    theme(legend.position = "top")



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

##    3 - Contribution par dpt    ================

#user_shape.shp contient la geométrie et les users
user_france.shp <- st_join(user_france.shp, dpt.shp["name"]) 

# on a des NA (85) verification :

user_na.shp <- user_france.shp %>% 
    filter(is.na(name))

plot(st_geometry(france.shp))
plot(st_geometry(user_na.shp), add = T, col = "red")

# je vais dropper les NA, le pb vient que je fais le tri sur le shape des communes simplifié 
# pas le meme que celui des dpts

user_france.shp <- user_france.shp %>% 
    filter(!is.na(name))


# la majorité des points sont genevois donc devrait 

# histograme contributeurs contribuant dans un ou plus de un departements
user_france.shp %>% 
    st_set_geometry(value = NULL) %>% # on drop la geométrie pour aller plus vite
    group_by(nom) %>% # on group pas nom
    summarise(dpt_nombre = n_distinct(name)) %>%  # on compte par dpt distincts
    ggplot() +
    geom_histogram(aes(x = dpt_nombre), binwidth = 1, fill = "darkblue") +
    labs( y = "Nombre de contributeurs",
          x = "Nombre de départements de contributions", 
          caption = "© Contributeurs OpenStreetMap "
        ) + 
    theme_bw()

# Une approximation des users par dpt

# ici on peut obtenir un nombre d'arbres par dpt par user

dpt_user.shp <- dpt_simplify.shp %>% 
                left_join(user_france.shp %>% # left join pour joindre via le nom de dpt
    st_set_geometry(value = NULL) %>% # on drop la geométrie pour aller plus vite, ici pe benchmarquer si on gagne du tenps `a `
    group_by(name) %>% 
    summarise(nbr_contrib = n_distinct(nom)), by = c("name" = "name"))

# on affiche les données
dpt_user.shp$nbr_contrib

summary(dpt_user.shp$nbr_contrib)

# un histo pour tester
ggplot(dpt_user.shp, aes( x = nbr_contrib)) +
    geom_histogram(breaks = seq(0, 200, by = 25), colour = "white")

tm_shape(dpt_user.shp) +
    tm_borders("grey", lwd = 1) +
    tm_shape(st_centroid(dpt_user.shp)) +
    tm_bubbles(col = "darkblue", size = "nbr_contrib", scale = 2, alpha = 0.5, 
               border.alpha = 0,  size.lim = c(min(dpt_user.shp$nbr_contrib, na.rm = T),200), 
               sizes.legend = seq(25, 225, by = 100), title.size = "Nbr. de contributeurs",
               legend.size.is.portrait = TRUE) +
    tm_scale_bar(position = c("center", "bottom")) +
    tm_credits("© OpenStreetMap contributors", size = 0.5, position=c("left", "top")) +
    tm_layout(legend.position = c("left","bottom"))

plot(dpt.shp)

# sauvegarde de l'information

temp_user <- user_france.shp %>% 
    st_set_geometry(value = NULL) %>% # on drop la geométrie pour aller plus vite
    group_by(nom) %>% # on group pas nom
    summarise(dpt_nombre = n_distinct(name))

liste_user.dat <- liste_user.dat %>%
    left_join(temp_user, by = "nom")

## FIN: se deconnecter de la base ===================

dbDisconnect(con)