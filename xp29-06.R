##.###################################################################################33
## Script rapide pour regarder le json de l'xp ====
##.#################################################################################33

library(jsonlite) # pour les json
library(tidyverse) # envt pour des donnees de type tidy pas trop volumineuse
library(sp) # ancien package spatial toujours présent
library(sf) # package spatial
library(lubridate) # les dates 
library(tmap) # cartes statics
library(tmaptools) # outils de carte dont palette_color
library(leaflet) # cartes dynamiques
library(ggmap) # outils pour les cartes statics avec tuiles
library(gganimate) # animation avec ggplot et map
library(spatstat) # outils d'analyse point pattern
library(maptools) # des outils principalements de conversion


### stream du json
exp_brut <- stream_in(file("data/tracesBrutesStEtienne29_06_19.json"))


#### rapides descriptions
summary(exp_brut)
str(exp_brut, max.level = 2) # faire fluctuer le level pour descendre dans list/df 
names(exp_brut)
# "_id"      "event"    "object"   "userId"   "username" "date"     "__v"      "activity"
dim(exp_brut)
class(exp_brut)

# . -------------------------------------------------------------------------- =============
# I - Nettoyage / mise en forme ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

# je met de coté "event" Pierre-Yves et Ludovic doivent être plus apte pour travailler avec ce type d' info
# mais je vais pe l'utiliser pour filtrer 
table(exp_brut$event, exp_brut$activity$index)

sum(table(exp_brut$event, exp_brut$activity$index)) 
# 935 on a des NA

unique(exp_brut$event)
unique(exp_brut$activity$name)
unique(exp_brut$activity$index)

# correspondance entre champs 
# activity$index : activity$name
# NA : verifier
# 0 : "Faites 5 relevés"
# 1 : "Faites 3 relevés de genres différents" 
# 2 : "Vérifiez 3 relevés d'un autre utilisateur (cercle vert clignotant)"
# 3 : "Identifiez 3 relevés d'expert (cercle bleu clignotant)"
# 4 : "Faites un maximum de relevé en un temps limite"
# il n'est utile que d'en garder que un

unique(exp_brut$"__v") 

# ne renvoie que 0 penser à le filtrer 

# userId et username pareil

unique(exp_brut$username)

# il faut me virer (defuneste)  car je n'ai pas participé, juste montré directement sur mon tel parfois

exp_brut <- exp_brut[exp_brut$username != "defuneste",]

# "date" On est pas tout à fait sur le bon creneau horaires, la tz doit pas être bonne je pense quil faut rajouter 2 h 
# on est pas non plus obliger de garer la date et on peut juste travailler en hms

table(exp_brut$event, exp_brut$activity$index)

## 1 - Recup des données ================

zone.shp <- st_read("data/zone_se.shp") # ouverture du fichier de zone 
st_crs(zone.shp) # vérifie le le CRS de la couche
summary(zone.shp) # verif de base

# on ne garde qu'une zone
zone.shp <- zone.shp[zone.shp$id ==1, ]
plot(zone.shp)

# on prend les arbres que l'on connait 
arbre_xp.shp <- st_read("data/arbres_se_final.geojson")
# limité à la zone
arbre_xp_zone.shp <- arbre_xp.shp[zone.shp,]

# on ne garde que ceux dans les limites de zones, attention je suis en lat/long

# . -------------------------------------------------------------------------- =============
# II - Je ne prends que "newObservation" ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

# on recode activité
exp_brut$code_activ <- exp_brut$activity$index 
exp_brut$code_activ <- exp_brut$code_activ+1

# sélection de newObservation
newObservation <- exp_brut[exp_brut$event == "newObservation",]
dim(newObservation)
str(newObservation, max.level = 2)
names(newObservation)

newObservation.df <- newObservation[,c("username","date","code_activ")] ## attention ici j'ai fait des selections par noms de colonnes
# on met la bonne tz 
attr(newObservation.df$date, "tzone") <- "Europe/Paris"

#### c'est assez hideux mais je fais vite
# on prend la date via une boucle, pe le changer 
newObservation$point <- NULL

for(i in 1:length(newObservation$event)) {
    newObservation.df$point[i] <- st_sfc(st_point(newObservation$object[[i]]$location$coordinates))}

newObservation.df <- st_sf(newObservation.df, geom = newObservation.df$point) # la bonne colone pour le champs geom
newObservation.df <- newObservation.df[,-4] ## attention ici j'ai fait des selections par num de colonnes

st_crs(newObservation.df) = 4326 # le bon scr

### ici c'est une solution R : on applique la fonction '[' (qui indexe la list), en fonction d'un vecteur de nom 
# le t() est juste pour un transpose
# as.data.frame est pour en faire un df

df_bota <- as.data.frame(t(sapply(newObservation[["object"]], `[`, c("authorName","common" , "specie", "genus"))))
names(df_bota) <- c("authorName", "common", "specie", "genus")

newObservation.df <- bind_cols(newObservation.df, df_bota) %>% select(-"authorName")

# ici il n'y a que trois variable exportées, pb  
st_write(newObservation.df, "data/newObservation.geojson")

## 1 - Une carte des nouvelle obs  =======

pal <- colorFactor(palette =c(get_brewer_pal("Set2", n = 7)),
                   levels = c("Catherine JHG", "JitenshaNiko", "Kasia Ozga", "MathDu", "pofx", "tjoliveau",  "Yoann Duriaux"),
                   na.color = "black")

carto_SE <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = arbre_xp_zone.shp, popup = arbre_xp_zone.shp$species, radius = 1) %>% 
    addCircleMarkers(data = newObservation.df, radius = 2, opacity = 0.7, 
                     popup = paste("Nom commun:", newObservation.df$common, "<br>",
                                   "Sp.:", newObservation.df$specie, "<br>",
                                   "Genre:", newObservation.df$genus),
                     color = ~pal(username)) %>% 
addLegend(position = "bottomright",
          pal = pal,
          values = c("Catherine JHG", "JitenshaNiko", "Kasia Ozga", "MathDu", "pofx", "tjoliveau",  "Yoann Duriaux"),
          na.label = "abs d'info")

carto_SE


# une carte animée

# il faut aller dans gif_xp.R

## 2 - Temporalité ==============
# à travailler

plot(newObservation.df$date)

strftime(newObservation.df$date, format="%H:%M:%S")

## 3 - Activité/personne ================

table(newObservation.df$username, newObservation.df$code_activ)

newObservation.df %>% 
ggplot(aes(x = code_activ, fill = username)) +
    geom_bar(position = "dodge") +
    labs(x ="Activités",
         y ="Nombre de relevés") 

# . -------------------------------------------------------------------------- =============
# II - On refait une carte  ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============


## 1 - Recup des données ================


# on ne garde que ceux dans les limites de zones, attention je suis en lat/long

plot(st_geometry(arbre_xp_zone.shp))

summary(arbre_xp_zone.shp)

## 2 - nndist ================

newObservation_zone.df <- newObservation.df[zone.shp,]

## ici on passe en sp avec sf
xp_sp <- as(st_transform(newObservation_zone.df, 2154), "Spatial")
## ici on passe en ppp avec maptools
xp_ppp <- as.ppp(xp_sp) 

## on verifie 
class(xp_ppp)
str(xp_ppp)

## on plot
plot(xp_ppp$x, xp_ppp$y)
# ici juste dans un veteur
# nndist vient de spatstat 
arbre_plus_proche <- nndist(xp_ppp)
class(arbre_plus_proche)
length(arbre_plus_proche)
head(arbre_plus_proche)

## on sauve comme une nouvelle variable
newObservation_zone.df$dist <- nndist(xp_ppp)

# un graph
newObservation_zone.df %>% 
    ggplot(aes(dist)) + # il faut un facteur pour utiliser colours
    geom_freqpoly(binwidth = 1) + 
    xlab("distance (m)") +
    ylab("Nombres d'arbres")