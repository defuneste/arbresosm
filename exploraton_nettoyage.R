### projet pour l'analyse des arbres seul dans OSM 
# exploration et "nettoyage des données"
# octobre 2018

# chargement des différents packages demandés 

### DB
library(RPostgreSQL) # fait le lien avec postgre, utilise DBI

### manip
library(dplyr) # manip de données en tidyverse
library(tibble)

### visualisation
library(ggplot2) # la visualisation

## analyse spatiale / carto
library(sp) # classes et methodes pour données spatiales pe déclassé par SF
library(rgdal) #gdal pour projection
library(rgeos) # geos, penser à installer libgeos++-dev avant
library(sf) # nouveau package de classes et methodes spatiales

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

# il faut convertir les infos dans hstore en un tableau
# je me suis inspiré de cette réponse : 
# https://dba.stackexchange.com/questions/94717/dynamically-convert-hstore-keys-into-columns-for-an-unknown-set-of-keys/123006
# elle fonctionne en deux temps
# en premier une requête qui va generer une requete avec chaque tag dans un SELECT
# ex : h->'addr:city' AS "addr:city" mais autant de fois que j'ai de clefs
# j'ai du utiliser $$ car j'avais des ' un peu partout 
# la base est l' utilisation de format 
# https://www.postgresql.org/docs/current/static/functions-string.html#FUNCTIONS-STRING-FORMAT

query <- "
SELECT format($$SELECT osm_id, h->%s 
	SELECT osm_id, tags AS h 
	FROM planet_osm_point
	WHERE planet_osm_point.natural = 'tree') t;$$
	, string_agg(quote_literal(key) || ' AS ' || quote_ident(key), $$, h->$$))
	AS sql   
FROM  (
   SELECT DISTINCT key
   FROM  planet_osm_point, skeys(tags) key
   WHERE planet_osm_point.natural = 'tree'
   ORDER  BY 1
   ) sub;"

# sql <- dbGetQuery(con, query)
# Query <- cat(shQuote(sql), "\n")
# cela ne marche pas, il faudra corriger
# j' ai corrigé le code dans pgadmin et créer une table propre
# que l'on va importer

arbretemp_tags <- dbGetQuery(con, "SELECT * FROM arbres_osm_tags;")
dim(arbretemp_tags)
names(arbretemp_tags)

# on groupe les deux tables
arbres_osm <- full_join(arbretemp, arbretemp_tags, by = "osm_id") #ici un bind_col pourrair aussi marcher
dim(arbres_osm)
names(arbres_osm) # le fichier est bien volumineux 

names_champs <- arbres_osm %>% 
    summarise_all(funs(sum(!is.na(.)))) %>% 
    t()  
    
dim(names_champs) # petite verif

names_champs <- rownames_to_column(as.data.frame(names_champs), var = "champs") # on passe les noms de champs dans une variable
 
barplot(sort(names_champs$V1, decreasing = T)) # un graphique rapide

names_champs <- names_champs %>% 
    arrange(desc(V1)) # on range par ordre decroissant

names_champs <- names_champs[-c(1:4),] # on retire les valeurs toujours présentent

names_champs[1:30,] %>% # un graph des 30 champs les plus présents
    ggplot( aes(x = reorder(champs, V1), y = V1)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Nombres d'arbres") +
    xlab("Champs")

### un import des regions et de la france
## on a toutes les petites îles

france.shp <- st_read(con,  query = "SELECT name, way
FROM planet_osm_polygon
WHERE boundary = 'administrative'  AND admin_level = '4';")


plot(france.shp)
st_crs(france.shp)

#### on va regarder pour les espèces



# se deconnecter de la base

dbDisconnect(con)