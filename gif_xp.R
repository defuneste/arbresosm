##.###################################################################################33
## Script rapide pour faire des gifs sur l'xp ====
##.#################################################################################33


# . -------------------------------------------------------------------------- =============
# I - Nettoyage / mise en forme ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

## 1 - load package  =======

library(jsonlite) # pour les json
library(tidyverse) # envt pour des donnees de type tidy pas trop volumineuse
library(sf) # package spatial
library(lubridate)
library(tmap)
library(tmaptools)
library(ggmap)
library(gganimate)
library(dplyr)

## 2 - stream du json et ouverture d'autre données necessaire  =======

zone.shp <- st_read("data/zone_se.shp") # ouverture du fichier de zone 
zone.shp <- zone.shp[zone.shp$id ==1, ] # on ne garde qu'une zone

arbre_xp.shp <- st_read("data/arbres_se_final.geojson") # on prend les arbres que l'on connait 

exp_brut <- stream_in(file("data/tracesBrutesStEtienne29_06_19.json")) #stream du json

# on recode activité
exp_brut$code_activ <- exp_brut$activity$index 
exp_brut$code_activ <- exp_brut$code_activ+1

# il faut me virer (defuneste)  car je n'ai pas participé, juste montré directement sur mon tel parfois
exp_brut <- exp_brut[exp_brut$username != "defuneste",]

# on ne garde que newObservation
newObservation <- exp_brut[exp_brut$event == "newObservation",]

newObservation.df <- newObservation[,c(5:6,9)] ## attention ici j'ai fait des selections par num de colonnes

#### c'est assez hideux mais je fais vite

newObservation$point <- NULL

for(i in 1:length(newObservation$event)) {
    newObservation.df$point[i] <- st_sfc(st_point(newObservation$object[[i]]$location$coordinates))}

newObservation.df <- st_sf(newObservation.df, geom = newObservation.df$point) # la bonne colone pour le champs geom
newObservation.df <- newObservation.df[,-4] ## attention ici j'ai fait des selections par num de colonnes

st_crs(newObservation.df) = 4326 # le bon scr

# ici j'extrait de manière un peu plus propre des infos comme nom commun, espèces et genus 

df_bota <- as.data.frame(t(sapply(newObservation[["object"]], `[`, c("authorName","common" , "specie", "genus"))))
names(df_bota) <- c("authorName", "common", "specie", "genus")

newObservation.df <- bind_cols(newObservation.df, df_bota) %>% select(-"authorName")

# on met la bonne tz 
attr(newObservation.df$date, "tzone") <- "Europe/Paris"

## 3 - on coupe pour avoir une bonne zone  =======

newObservation.df <- newObservation.df[zone.shp,] # les obs
arbre_xp_zone.shp <- arbre_xp.shp[zone.shp,] # les arbres attendus

# . -------------------------------------------------------------------------- =============
# II - une carte animée ----------------------------------------------------------------- =============
# . -------------------------------------------------------------------------- =============

## 1 - Fond de cartes  =======
# ici je prend chez stamen, il faut une bbox bb() de sf 
xp_st_e <- ggmap(get_stamenmap(bb(zone.shp, output = "matrix"),zoom = 16, maptype = "terrain-lines"))

xp_st_e +
    


# str(xp_st_e)
# bb(x = xp_st_e$data$lon, y = xp_st_e$data$lat)

arbre_xp_zone.coord <-st_coordinates(arbre_xp_zone.shp)
arbre_xp_zone.coord <- as.data.frame(arbre_xp_zone.coord)

arbre_xp_zone.shp[bb(x = xp_st_e$data$lon, y = xp_st_e$data$lat),]

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
    geom_point(aes(x = 4.3860717, y = 45.4496287), size = 4, pch = "M") +# localisation mixeur
    geom_point(data = arbre_xp_zone.coord, aes(x = X, y = Y), size = 0.75, col = "#208842", alpha = 0.5) +
    #geom_point(data = obs_timing, aes(x = X, y = Y), size = 2.5) + 
    geom_point(data = obs_timing, aes(x = X, y = Y, colour = participant), size = 2.5) + 
    xlab("") + ylab("") +
    ggtitle("Test d'Albiziapp",
            subtitle = 'time:{frame_time}') +
    transition_components(date) +
    shadow_mark() 

xp_st_e_anim

anim_save("xp_st_e_anim.gif" , animation = last_animation())


