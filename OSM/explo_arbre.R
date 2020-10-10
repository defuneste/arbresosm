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

time_user.dat <- st_read(con,  query = "select osm_timestamp from points;")

time_user.dat$osm_timestamp <- as.Date(time_user.dat$osm_timestamp)
# on peut aussi regarder les heures de cartographie mais attention au decalage horaires

ggplot(user.shp, aes(x = osm_timestamp)) +
    geom_histogram(binwidth = 15) +
    xlab("Années") +
    ylab("Nb. arbres isolés") +
    labs(caption ="source : © les contributeurs d’OpenStreetMap")


user.dat <- st_read(con,  query = "select osm_uid from points;")
head(user.dat)

# liste des utilisteurs avec le nombre d'arbres
liste_user.dat <- user.dat %>% 
    group_by(osm_uid) %>% 
    summarise(nb_arbre = n()) %>% 
    arrange(nb_arbre) 

liste_user.dat$cumsum_nbarbre <- cumsum(liste_user.dat$nb_arbre)
sum(liste_user.dat$nb_arbre) == max(liste_user.dat$cumsum_nbarbre)

# calcul d'un indice de gini
liste_user.dat <- liste_user.dat %>% 
    mutate(order = 1:length(liste_user.dat$osm_uid)) 

liste_user.dat <- liste_user.dat %>% 
    mutate(pourcent_order = order/max(liste_user.dat$order)*100,
       pourcent_arbre = cumsum_nbarbre/max(liste_user.dat$cumsum_nbarbre)*100)

ggplot(liste_user.dat, aes(x = order/max(order), y = cumsum_nbarbre/max(cumsum_nbarbre))) +
    geom_path(aes(colour = "forestgreen"), size = 1.5) +
        scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs( y = "Pourcentage d'arbres ajoutés",
          x = "Pourcentage de contributeurs", 
          caption = "© Contributeurs OpenStreetMap ") + 
    scale_colour_manual(name = '', 
                        values =c("forestgreen"="forestgreen","gray30"="gray30"), labels = c("Arbres isolés","Objets OSM")) +
    theme_bw() +
    theme(legend.position = "top")


dbDisconnect(con)