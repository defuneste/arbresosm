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
    addCircleMarkers(data = newObservation.df, radius = 2, opacity = 0.7,
                     color = ~pal(username)) %>% 
addLegend(position = "bottomright",
          pal = pal,
          values = c("Catherine JHG", "JitenshaNiko", "Kasia Ozga", "MathDu", "pofx", "tjoliveau",  "Yoann Duriaux"),
          na.label = "abs d'info")

carto_SE

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
