### projet pour l'analyse des arbres seul dans OSM 
# exploration et "nettoyage des données"
# octobre 2018

# chargement des différents packages demandés 

library(RPostgreSQL) # fait le lien avec postgre, utilise DBI
library(sp) # classes et methodes pour données spatiales
library(dplyr) # manip de données en tidyverse

# il faut établir une connexion 

pw <- {
  "osm117" # oui c'est pas top de l'ecrire
}

# charge les drivers pour postgre 
drv <- dbDriver("PostgreSQL")
# class(drv) #une verif

# fais un pont vers la db réutilisable
# ici j'ai pris une db en local pour tester
# con sera utilisé pour chaque connection et pkoi le franciser
con <- dbConnect(drv, dbname = "osmdbfrance",
                 host = "localhost", port = 5432, # attention 5432 par défaut
                 user = "postgres", password = pw) # idem pour user
rm(pw) # mouais

# vérifie pour une table 
dbExistsTable(con, "planet_osm_point") 

# une requete pour la route
query <- "SELECT COUNT(p.natural)
          FROM planet_osm_point AS p
          WHERE p.natural ='tree';"

# on execute la requete
df_arbres <- dbGetQuery(con, query)
df_arbres


# les differents tags : skey() renvoie la valeur de tous les attributs d'un hstore comme un set 
# https://www.postgresql.org/docs/current/static/hstore.html

querytags <- "SELECT DISTINCT skeys (tags), COUNT(*) AS decompte
              FROM planet_osm_point
              WHERE planet_osm_point.natural = 'tree'
              GROUP BY skeys (tags)
              ORDER BY decompte DESC;"

nom_tags <- dbGetQuery(con, querytags)
dim(nom_tags)

# les autres champs que tag


querychamps <- "SELECT * 
                FROM planet_osm_point
                WHERE planet_osm_point.natural = 'tree';"

nom_champs <- dbGetQuery(con, querychamps)
dim(nom_champs) # un peu d'info
str(nom_champs) # un peu d'info

names_champs <- nom_champs %>% # on prends le total
    summarise_all(funs(sum(!is.na(.)))) # on compte ceux renseignés

sum(names_champs > 0) # retourne le nombre de champs renseignés
                      # attention tags et natural sont gardés

# on garde les tags et les champs dans un seul df

arbretemp <- nom_champs[,names_champs > 0] # on ne garde que les champs renseignés




# se deconnecter de la base

dbDisconnect(con)