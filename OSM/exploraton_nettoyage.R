### projet pour l'analyse des arbres seul dans OSM 
# exploration et "nettoyage des données"
# octobre 2018
# Petites notes sur le code :
# pour me rapeller qu'un objet à des infos geometriques type vecteur je lui accolle un .shp
# si c'est un raster .grid, si c'est un dataframe souvent un .dat

# chargement des différents packages demandés 

source("OSM/connect_db_france.R")

## I Premier Niveau de Description =========================================

# I.1 un petit exemple ====================================================
# combien il y a d'arbres dans la bd ? 
query <- "SELECT COUNT(p.natural)
          FROM planet_osm_point AS p
          WHERE p.natural ='tree';"

# on execute la requete
df_arbres <- dbGetQuery(con, query)
df_arbres

# les clefs/valeur assosicés aux arbres ================
# les differents tags : skey() renvoie la valeur de tous les attributs d'un hstore comme un set 
# https://www.postgresql.org/docs/current/static/hstore.html

querytags <- "SELECT DISTINCT skeys (tags), COUNT(*) AS descompte
              FROM planet_osm_point
              WHERE planet_osm_point.natural = 'tree'
              GROUP BY skeys (tags)
              ORDER BY descompte DESC;"

nom_tags <- dbGetQuery(con, querytags)
nom_tags$type_de_field <- "hstore"
dim(nom_tags)



# les autres champs que tag

querychamps <- "SELECT * 
                FROM planet_osm_point
                WHERE planet_osm_point.natural = 'tree';"

nom_champs <- dbGetQuery(con, querychamps)
dim(nom_champs) # un peu d'info
str(nom_champs) # un peu d'info

names_champs <- nom_champs %>% # on prends le total
    dplyr::summarise(
        dplyr::across(dplyr::everything(),
                      ~ sum(!is.na(.x) # is on compte tout ce qui n'est pas NA
                            )
                      )
        ) %>% 
    tidyr::pivot_longer(cols = dplyr::everything(), # la transposition sur tout les colonnes
                 names_to = "skeys",
                 values_to = "descompte") %>% 
    dplyr::filter(descompte != 0) %>% 
    dplyr::mutate(type_de_field = "champs") 

descompte_champs <- names_champs %>% 
                        dplyr::bind_rows(nom_tags) %>% 
                        dplyr::arrange(dplyr::desc(descompte))

# openxlsx::write.xlsx(descompte_champs, "exports/nom_tags.xlsx")

# il faut convertir les infos dans hstore en un tableau
# je me suis inspiré de cette réponse : 
# https://dba.stackexchange.com/questions/94717/dynamically-convert-hstore-keys-into-columns-for-an-unknown-set-of-keys/123006
# elle fonctionne en deux temps : en premier une requête qui va generer une requete et en second va l'executer
# 
#la base est l' utilisation de format 
# https://www.postgresql.org/docs/current/static/functions-string.html#FUNCTIONS-STRING-FORMAT
# qui produit une chaine de caractères qui correspond à ma futur requete
#
# Premier temps a plusieurs requêtes nichées la première produit un tableau qui garde osm_id et tags (la liste) :
#       SELECT osm_id, tags->%s 
#       FROM planet_osm_point
#       WHERE planet_osm_point.natural = 'tree' ; #
# puis pour chaque valeur unique / et ordre alphabetique (le FROM): 
#       SELECT DISTINCT key
#       FROM  planet_osm_point, skeys(tags) key
#       WHERE planet_osm_point.natural = 'tree'
#       ORDER  BY 1 
# et on specifie le format via string_agg pour suivre ceci : 
# h->'addr:city' AS "addr:city" mais autant de fois que j'ai de clefs
# j'ai du utiliser $$ car j'avais des ' un peu partout 
# 

query <- "
SELECT format($$ SELECT osm_id, tags->%s 
	FROM planet_osm_point
	WHERE planet_osm_point.natural = 'tree' ; $$
	, string_agg(quote_literal(key) || ' AS ' || quote_ident(key), $$, tags->$$))
	AS arbre_tag_sql   
FROM  (
   SELECT DISTINCT key
   FROM  planet_osm_point, skeys(tags) key
   WHERE planet_osm_point.natural = 'tree'
   ORDER  BY 1 
   ) sub;"

# second temps executer la query c'est un peu brute

arbre_tag_sql <- dbGetQuery(con, query) # retoune la requête à lancer
arbretemp_tags <- dbGetQuery(con, arbre_tag_sql[1,1]) # ici il faut prendre la première ligne/colonne

## il est possible de vérifier les changeset ave ce format "https://www.openstreetmap.org/changeset/<Changeset number>"
## il est possible de verifier les nodes avec ce format : https://www.openstreetmap.org/node/<numéro de node>

names_champs <- names_champs[-c(1:9),] # on retire les valeurs toujours présentes

names_champs[1:30,] %>% # un graph des 30 champs les plus présents
    ggplot( aes(x = reorder(champs, V1), y = V1)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(caption ="source : © les contributeurs d’OpenStreetMap") +
    ylab("Nombres d'arbres") +
    xlab("Champs")

### un import des regions et de la france
## on a toutes les petites îles
# on a un coin suisse et pas les iles anglo normandes sur data gouv

france.shp <- st_read(con,  query = "SELECT name, way
FROM planet_osm_polygon
WHERE boundary = 'administrative'  AND admin_level = '4';")

str(france.shp, max.level = 2) # on regarde un peu l'objet

plot(france.shp) # on a un paquet de petites Iles non fr dans le cas de geofabrik
st_crs(france.shp) # WGS 84 / Pseudo-Mercator

plot(france.shp[france.shp$name == "Solothurn",]) # une petite verif sans NSE

france.shp %>% 
filter(name == "Formica III") # une version avec NSE

france.shp$surface_ha <-  set_units(st_area(france.shp), value = ha) # on calcul la surface en ha

# on filtre par la surface pour enlever les paradis fiscaux
# on a encore un problème avc des îles italiennes aux larges de la corse, on vire Toscanna dans les données geofabrik
# il y a un bout de la suisse dans les données data gouv
# les objets units doivent être filtrés ou indexés par d'autre objets units d'ou le set_units(x, value = ha)
# pas certains que dplyr aime du coup c'est du r:base
plot(france.shp[france.shp$name != "Genève" & france.shp$surface_ha > set_units(30000, value = ha),])
france.shp <- france.shp[france.shp$name != "Genève" & france.shp$surface_ha > set_units(30000, value = ha),]

## faire des cartes avec tmap

# un import plus leger avec les espèces
species.shp <- st_read(con,  query = "SELECT way, tags -> 'species' AS species, tags -> 'genus' AS genus
FROM planet_osm_point
WHERE planet_osm_point.natural = 'tree';")

class(species.shp) # on vérifie la classe

st_crs(species.shp) == st_crs(france.shp) # petite verif sur CRS


species.shp$especes <- factor(ifelse(is.na(species.shp$species), "Non renseigné", "Renseigné"))
str(species.shp$especes)


tm_shape(st_simplify(st_geometry(france.shp)), dTolerance = 100) + # attention il a un simplify pour aller plus vite
    tm_borders("grey") +
    tm_shape(species.shp) +
        tm_dots(alpha = 0.4, col = "especes", palette = c("#8be0b3", "red"), title = "Key:species") +
    tm_scale_bar(position = c( "center", "BOTTOM"))



#### on va regarder pour les espèces
# il y a plusieurs attributs pouvant contenir l'info au niveau des espèces

# l'attribut species

length(unique(species.shp$species)) ## il y a 1814 valeures différentes à "species"

# on va dropper la geometry pour aller plus vite 

species.dat <- st_set_geometry(species.shp, value = NULL)
class(species.dat) # un df

unique(species.dat$species)

# vu la diversité on va passer par un cas particulier (les platanes) que l'on va pe étendre 

# un premier grep pour se rapprocher 
temp <- unique(species.dat$species)[grep(pattern = "atan", unique(species.dat$species))]  
# on enleve les érables, tillieul et robinier
temp[grep(pattern = "^A|T|R", temp, invert = T)]

# production d'un tableau d'effectifs des différents platanes
platanes <- species.dat %>%
    filter(species %in% temp[grep(pattern = "^A|T|R", temp, invert = T)]) %>%
    group_by(species) %>%
    summarize(comptage = n()) %>%
    arrange(desc(comptage))  %>%
    print(n = Inf)

sum(platanes$comptage)

### présences des " arbres" de reveries dans OSM

esp <- read.csv("data/nomsp_nomverma.csv", sep = "\t") # sp. de reveries

ranking <- species.dat %>%
    filter(species %in% esp$Species) %>%
    group_by(species) %>%
    summarize(comptage = n()) %>%
    arrange(desc(comptage))

openxlsx::write.xlsx(ranking, "data/rangking.xls")

### présences des arbres par land use 

# import du corine land cover de 2012, source espace EVS
luluc.shp <- st_read("CLC12_FR_RGF.shp")

sort(unique(luluc.shp$CODE_12))

# une variable au niveau du land cover 

luluc.shp <- luluc.shp %>%
    mutate(CODE_12 = as.numeric(levels(CODE_12))[CODE_12],
        niv1 = case_when(
        CODE_12 < 200 ~ "1",
        CODE_12 > 200 & CODE_12 < 300 ~ "2",
        CODE_12 > 300 & CODE_12 < 400 ~ "3",
        CODE_12 > 400 & CODE_12 < 500 ~ "4",
        CODE_12 > 500 ~ "5"
    ))

attributes(luluc.shp)
st_agr(luluc.shp) <- c("identity", "constant", "aggregate", "constant")

head(luluc.shp)

# luluc_simplify.shp <- ms_simplify(luluc.shp, keep = 0.25,
#                                  keep_shapes = TRUE)

luluc_niv1  <- luluc.shp %>%
    select(niv1) %>%
    group_by(niv1) %>%
    summarise(compte = n()) 
luluc_niv1 

plot(luluc_niv1)

st_write(luluc_niv1, "luluc_niv1.shp")


### circonference 

unique(arbres_osm$circumference)

### diamètre de la couronne 

unique(arbres_osm$diameter_crown)
table((as.integer(arbres_osm$diameter_crown)))
hist(as.integer(arbres_osm$diameter_crown), breaks = 1:max(as.integer(arbres_osm$diameter_crown), na.rm = T), xlim = c(0,30))

### hauteur

unique(arbres_osm$height)
table((as.integer(arbres_osm$height)))
hist(as.numeric(arbres_osm$height)[!is.na(as.numeric(arbres_osm$height))], breaks = 0:125, xlim = c(0,40), main = "hauteur")

#, xlim = c(0,30)

### start_date

unique(arbres_osm$start_date)
unique(arbres_osm$`start_date:FR:plantation`)






# se deconnecter de la base

dbDisconnect(con)