##.###################################################################################33
## Script rapide pour regarder le json de l'xp ====
##.#################################################################################33

library(jsonlite) # pour les json
library(tidyverse) # envt pour des donnees de type tidy pas trop volumineuse
library(sf) # package spatial
library(purrr) # functional prog
library(lubridate)
library(tmap)
library(tmaptools)
library(leaflet)
library(ggmap)
library(gganimate)


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

# . -------------------------------------------------------------------------- =============
# II - Je ne prends que "newObservation" ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

# on recode activité
exp_brut$code_activ <- exp_brut$activity$index 
exp_brut$code_activ <- exp_brut$code_activ+1

newObservation <- exp_brut[exp_brut$event == "newObservation",]
dim(newObservation)
str(newObservation, max.level = 2)

newObservation.df <- newObservation[,c(5:6,9)] ## attention ici j'ai fait des selections par num de colonnes

# on met la bonne tz 
attr(newObservation.df$date, "tzone") <- "Europe/Paris"

#### c'est assez hideux mais je fais vite

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


## 1 - Une carte des nouvelle obs  =======

pal <- colorFactor(palette =c(get_brewer_pal("Set2", n = 7)),
                   levels = c("Catherine JHG", "JitenshaNiko", "Kasia Ozga", "MathDu", "pofx", "tjoliveau",  "Yoann Duriaux"),
                   na.color = "black")

carto_SE <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = newObservation.df, radius = 2, opacity = 0.7, popup = newObservation.df$common,
                     color = ~pal(username)) %>% 
addLegend(position = "bottomright",
          pal = pal,
          values = c("Catherine JHG", "JitenshaNiko", "Kasia Ozga", "MathDu", "pofx", "tjoliveau",  "Yoann Duriaux"),
          na.label = "abs d'info")

carto_SE


# une carte animée

newObservation.df <- newObservation.df[zone.shp,]

xp_st_e <- ggmap(get_stamenmap(bb(zone.shp, output = "matrix"),zoom = 16, maptype = "terrain-lines"))

arbre_xp_zone.coord <-st_coordinates(arbre_xp_zone.shp)
arbre_xp_zone.coord <- as.data.frame(arbre_xp_zone.coord)


xp_st_e + geom_sf(data = newObservation.df,  inherit.aes = FALSE, pch = 16, alpha = 0.7, colour = username, size = 1)  

obs_timing <- st_coordinates(newObservation.df)
obs_timing <- as.data.frame(obs_timing)
obs_timing$date <- newObservation.df$date
obs_timing$username <- newObservation.df$username

obs_timing$participant <- "Particpant 1"
obs_timing$participant[obs_timing$username == "tjoliveau"] <- "Particpant 2"
obs_timing$participant[obs_timing$username == "JitenshaNiko"] <- "Particpant 3"
obs_timing$participant[obs_timing$username == "MathDu"] <- "Particpant 4"
obs_timing$participant[obs_timing$username == "Yoann Duriaux"] <- "Particpant 5"
obs_timing$participant[obs_timing$username == "pofx"] <- "Particpant 6"
obs_timing$participant[obs_timing$username == "Catherine JHG"] <- "Particpant 7"


unique(obs_timing$username)

xp_st_e_anim <- xp_st_e + 
    geom_point(data = arbre_xp_zone.coord, aes(x = X, y = Y), size = 0.75, col = "#208842", alpha = 0.5) +
    geom_point(data = obs_timing, aes(x = X, y = Y, colour = participant), size = 2.5) + 
            xlab("") + ylab("") +
    transition_time(date) +
    shadow_trail() +
    ggtitle("Test d'Albiziapp",
            subtitle = 'Frame {frame} of {nframes}')

xp_st_e_anim

## 2 - Temporalité ==============

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

zone.shp <- st_read("data/zone_se.shp") # ouverture du fichier de zone 
st_crs(zone.shp)
summary(zone.shp)

# on ne garde qu'une zone

zone.shp <- zone.shp[zone.shp$id ==1, ]
plot(zone.shp)

# on prend les arbres que l'on connait 

arbre_xp.shp <- st_read("data/arbres_se_final.geojson")

# on ne garde que ceux dans les limites de zones, attention je suis en lat/long

arbre_xp_zone.shp <- arbre_xp.shp[zone.shp,]

plot(st_geometry(arbre_xp_zone.shp))

summary(arbre_xp_zone.shp)

carto_SE %>% 
    addCircleMarkers(data = arbre_xp_zone.shp, popup = arbre_xp_zone.shp$species, radius = 1)