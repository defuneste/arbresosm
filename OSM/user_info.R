# Date: octobre 2018, cotobre 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: qui sont les osmiens qui ajoite des arbres ?
# Description du problème:
# On reste en France, connection via la debian de l'ujm
#
# Libraries utilisées: 
# "RPostgreSQL", "sf, "dplyr"


##.###################################################################################33
## I Chargement des différents packages demandés ====
##.#################################################################################33

# chargement des codes depend de l'ent  
source("OSM/connect_db_france.R")

### DB
library(sf) 

##.###################################################################################33
## II Ouverture et preprocessing des données ====
##.#################################################################################33

##    1 - Recup des Arbres OSM  ================

# import des conributeurs et du moment de la contribution
user.shp <- st_read(con,  query = "SELECT  way, 
                                           tags -> 'osm_timestamp' AS ts, 
                                           tags -> 'osm_user' AS nom, 
                                           tags -> 'osm_changeset' AS changeset,
                                           osm_id AS ID, 
                                           tags -> 'osm_uid' AS user_id, 
                                           tags -> 'osm_version' AS version
                       FROM planet_osm_point
                       WHERE planet_osm_point.natural = 'tree';")

##    2 - Recup des shapes utiles  ================

# chargement du scripts des limites administratives

dpt_L93.shp <- sf::read_sf("data/dpt_simp.geojson") %>% 
                sf::st_transform(2154)

regions.shp <- sf::st_read("data/regions_simp.geojson")

france.shp <- sf::st_union(regions.shp)

##    3 - Preprocessing des données  ================

#on avait 1 493 197 natural = tree sur l éxport france 

user_france.shp <- user.shp[france.shp,] # je coupe avec la france

# on est passé à 1400070

user.dat <- sf::st_drop_geometry(user_france.shp)

user.dat$ts <- as.Date(user.dat$ts) # on passe en POSIX juste date

write.csv(user.dat, "data/user.csv")

##.###################################################################################33
## III Travail sur osm_user et sur l'encodage des arbress ====
##.#################################################################################33

library(ggplot2)

##    1 - Evolution dans le temps des arbres dans OSM   ================

# liste des utilisateurs avec le nombre d'arbres
liste_user.dat <- user.dat %>% 
    dplyr::group_by(nom) %>% 
    dplyr::summarize(nb_arbre = dplyr::n(),
              duree = max(ts) - min(ts)) %>% 
    dplyr::arrange(nb_arbre) # ici je classe par par ordre descendant car je vais utiliser plus loin pour faire une courbe de lorentz
liste_user.dat <- liste_user.dat %>% 
    dplyr::mutate(order = 1:length(liste_user.dat$nom)) 

##    2 - Contribution cumulée de chaques utilisateurs    ================

#  nombre d'utilisateurs contribuant aux arbres
dim(liste_user.dat)

median(liste_user.dat$nb_arbre) # mediane
mean(liste_user.dat$nb_arbre) # moyenne

liste_user.dat$cumsum_nbarbre <- cumsum(liste_user.dat$nb_arbre) # somme cumulée des arbres

# des pourcentages
liste_user.dat <- liste_user.dat %>% 
    dplyr::mutate(pourcent_order = order/max(liste_user.dat$order) * 100,
           pourcent_arbre = cumsum_nbarbre/max(liste_user.dat$cumsum_nbarbre) * 100)

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
    dplyr::group_by(nom) %>% 
    dplyr::summarise(comptage = sum(count)) %>% 
    dplyr::arrange(comptage) # ici je classe par par ordre descendant car je vais utiliser plus loin pour faire une courbe de lorentz

OSM_user_tot <- OSM_user_tot %>% 
    dplyr::mutate(order = 1:length(OSM_user_tot$nom)) 

OSM_user_tot$cumsum_nbarbre <- cumsum(OSM_user_tot$comptage) # somme cumulée des arbres

sum(OSM_user_tot$comptage) # nombre d'objet dans OSM france

# ici j' ai regarde les rangs
text_lorentz <- dplyr::slice(liste_user.dat, c(3864,3951,3977))
label_text_lorentz <- c("121 contributeurs", "34 contributeurs", "8 contributeurs")

# ici order est divisé par son max et pareil pour la somme cumuléé
# cela les passe de 0 à 1 et on modifie les labels avec 
# scales::percent
ggplot(liste_user.dat, 
       aes(x = order/max(order), 
           y = cumsum_nbarbre/max(cumsum_nbarbre))) +
    geom_path(aes(colour = "forestgreen"), 
              size = 1.5) +
    geom_line(data = OSM_user_tot,
              aes(x = order/max(order) , 
                                       y = cumsum_nbarbre/max(cumsum_nbarbre), colour = "gray30"), lty = 2) +
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

# j'ai pris la 15aine mais on est presque de l'ordre du jour
# ici sur un fichier léger

ggplot(user.dat, aes(x = ts)) +
    geom_histogram(binwidth = 15) +
    xlab("Années") +
    ylab("Nb. arbres isolés") +
    labs(caption ="source : © les contributeurs d’OpenStreetMap") +
    theme_bw()

# si on veut aller un peu plus loin et verifier d'ou vient la donnée on peut utiliser les champs refs
# il faut prendre cette base dans exploration, je devrais pe en faire une version r binary pour gagner 
# du temps

# un grep pour avoir les noms de variables qui commence par ref
import_bd <- names(arbres_osm)[grep(pattern = "^ref", names(arbres_osm))]

# ref est étrange et doit être un peu ausculté, il semble cependant bien que ce soit une ref d'une base

pas_na <- function(x) {sum(!is.na(x))} # une fonction qui somme les T/F sur des pas Na

# ici je compte les nom Na pour les refs
lapply(arbres_osm[names(arbres_osm)[grep(pattern = "^ref", names(arbres_osm))]], pas_na)

arbres_osm$refbd <- apply(arbres_osm[,import_bd], 1, pas_na)

table(arbres_osm$refbd)  # j'ai des chevauchement au max 3 et souvent 2 sur Paris

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

## constitution d'un tableau pour les profils d'utilisteurs : 

dbListTables(con)






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
    st_set_geometry(value = NULL) %>% # on drop la geométrie pour aller plus vite user
    group_by(name) %>% 
    summarise(nbr_contrib = n_distinct(nom)), by = c("name" = "name"))


nb_contrib_principal <- user_france.shp %>% 
    st_set_geometry(value = NULL) %>% # on drop la geométrie pour aller plus vite, ici pe benchmarquer si on gagne du tenps `a `
    group_by(nom, name) %>% # on regroupe par nom d'user et pas nom de dpt
    summarise(nbr_contrib = n()) %>%  # on compte dans ces categories
    group_by(nom) %>%  # on regroupe par nom
    filter(nbr_contrib == max(nbr_contrib)) %>% # on filtre en fonction de quand le nbr de contrib est égale au max
    # du coup attention il peut y avoir des egalités (ce qui me dérange pas on va assumer que le contributeur est sur les deux dpts)
    group_by(name) %>% 
    summarise(nb_contrib_principal = n()) 

# on ajoute le nombre de contributeurs principaux (ayant le plus de contribution d'arbre isolés là)  
# on pourrait essayer de la faire sur l'ensemble des contribution

dpt_user.shp <- dpt_user.shp %>% 
                    left_join(nb_contrib_principal, by = c("name", "name"))

# on elenve les non dpt fr
dpt_user.shp <- dpt_user.shp %>% 
    filter(!is.na(nbr_contrib))

dpt_user.shp$nb_contrib_principal[is.na(dpt_user.shp$nb_contrib_principal)] <- 0

# calcul des "visiteurs" 
dpt_user.shp$visit_contrib <- dpt_user.shp$nbr_contrib - dpt_user.shp$nb_contrib_principal


st_write(dpt_user.shp, "data/dpt.shp")

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
               border.alpha = 0,  size.max = 197, 
               sizes.legend = seq(50, 200, by = 50), title.size = "Nbr. de contributeurs",
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