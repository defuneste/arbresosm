# Date: octobre 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: nb d'arbre isolé en fonction du temps dans OSM
# Description du problème:
# Les CSV viennent d'une instance overpass en local 
#
# Libraries utilisées:
# sf, spData

# Il faut commencer par verifier les bbox associées à ces exports 
# pour verifier si elles se superposent 

library(sf)
library(spData)

#overpass bbox is S - W - N -E
# sf is xmin / xmax / ymax / ymin 
# xmin = W / X max = E / ymax = N / Y min = S

# reorg_bbox prend un vecteur au format SWNE et le passe en xmin/xmax/ymin/ymax
reorg_bbox <- function(x) {
    x <- c(xmin = x[2], xmax = x[4], ymin = x[3], ymax = x[1])
    return(x)
}

# une bbox sous forme de polygone 

do_me_a_bbox <- function(el_de_list) { 
    # une colonne de geom
    sf::st_as_sfc(sf::st_bbox(reorg_bbox(el_de_list),
                              crs = 4326)
                  )  
    
} 

# une liste des bbox prise par l'api d'overpass au format SWNE
bbox_list <- list(europe_op  = c(34, -13, 65, 48),
                  USA_op = c(24, -126, 51, -65),
                  Africa_op = c(36, -21, 39, 55),
                  Asia_op = c(11, 25, 77, 180),
                  NAmerica_op = c(9 ,-168, 73, -51),
                  SAmerica_op = c(-56, -90, 17, -31),
                  Australia_op = c(-47, 106, 8, 180) 
                  )

# des polygons en data frame j'ai perdu le format avec rbind
mes_bbox_sfc <- do.call(rbind, 
                        lapply(bbox_list, 
                               do_me_a_bbox))

# on repasse en sf
mes_bbox <- st_sf(nom = row.names(mes_bbox_sfc), 
                  geom = mes_bbox_sfc, 
                  crs = 4326)

plot(world$geom)
plot(mes_bbox,  add = T)

# pb avec l'Afrique ! et c'est un peu juste pour la zone pacifique !

# j'ai produit une liste de csv par des tas de bbox. Ils ne contiennent que l'id de la node 
# le nom de l'user et la timsteamp

# liste des path

liste_csv <- list.files("data/data_ts/", 
                        full.names = TRUE)

bbox <- lapply(liste_csv, 
               read.csv, 
               quote = "", sep = "\t", col.names = c("id", "user", "timestamp"))

bbox <- do.call("rbind", bbox)
bbox$timestamp <- as.Date(bbox$timestamp)

library(ggplot2)

ggplot(bbox, aes(x = timestamp)) +
    geom_histogram(binwidth = 15) +
    xlab("Années") +
    ylab("Nb. arbres isolés") +
    labs(caption ="source : © les contributeurs d’OpenStreetMap") +
    theme_bw()

# nombre d'users
length(unique(bbox$user))

count_by_user <- aggregate(id ~ user, data = bbox, FUN = length)

count_by_user <- count_by_user[order(count_by_user$id, 
                    decreasing = TRUE),]

names(count_by_user)[2] <- "Nb_arbres"

summary(count_by_user$Nb_arbres)