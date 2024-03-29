# Date: octobre 2020, reprise octobre 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: charger la base d'osm france postgres
# Description du problème:s
# La base est sur debian de l'université cf.
# maj_totale_france_journalisé.sh
# /!\ la deconnection n'est pas 
# Libraries utilisées:
# "RPostgreSQL", "sf" 

source("OSM/connect_db_france.R")

library(sf)

# soyons fou 

# cerveau de poule : tags est le champs hstore on le requete comme ceci tags -> 'key' as nouveau_nom

time_user.dat <- st_read(con,  query = "select tags -> 'osm_timestamp' as timestamp from planet_osm_point;")

time_user.dat$timestamp <- as.Date(time_user.dat$timestamp)
# on peut aussi regarder les heures de cartographie mais attention au decalage horaires

ggplot(time_user.dat, aes(x = timestamp)) +
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


#### 1- Exploration spatiale =============================================

# ouverture de shape pour les continents 
 source("OSM/shape_fonds.R")

# quelques tests
extrait_arbre <- st_read(con,  query = "select * from points limit 10;")
str(extrait_arbre)
# affichage 
plot(extrait_arbre$wkb_geometry)
#nb de points
count <- st_read(con,  query = "select count(*) from points;")

# count

test_continent <- st_read(con,  query = "select count(*), continent from points group by continent;")

sum(test_continent$count)
arbres.shp <- st_read(con,  query = "select * from points limit 10;")

dbExecute(con, "create table continent(
                    continent varchar(40),
                    geometry geometry(MULTIPOLYGON,  4326)
                    );")

dbWriteTable(con, 
             name = "continent",
             value = continent.shp, 
             overwrite = FALSE,
             append = TRUE, 
             binary = FALSE
            )

# une copie 
dbExecute("select * 
          into copy_arbre
          from points;")

# marche pas je sais pas pkoi
dbGetQuery(con, query = "select count(*), c.continent
    from continent as c
    join points as t
    on ST_Intersects(c.geometry, t.wkb_geometry)
    group by c.continent;")

dbExecute(con, query = "alter table points add continent VARCHAR(40);")

dbExecute("update points 
            set continent = (select c.continent 
            from points as t 
            join continent as c 
            on st_intersects(c.geometry, t.wkb_geometry)
            where points.ogc_fid = t.ogc_fid)
        ;")


continent_na.shp <- st_read(con, query = "select points.continent, points.wkb_geometry from points where continent is NULL;") 

nbr <- 100
echantillon <- sample(1:nrow(continent_na.shp), size = nbr)

library(mapview)
mapview(list(continent_na.shp[echantillon,],continent10a.shp))

st_crs(continent.shp)
# on sauve les continents dans la base




##.###################################################################################33
## III deconnection ====
##.#################################################################################33


dbDisconnect(con)