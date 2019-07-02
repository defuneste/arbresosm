##.###################################################################################33
## Script rapide pour regarder le json de l'xp ====
##.#################################################################################33

library(jsonlite) # pour les json
library(tidyverse) # envt pour des donnees de type tidy pas trop volumineuse
library(sf) # package spatial
library(purrr) # functional prog
library(tmap)
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
# mais je vauis pe l'utiliser pour filtrer 
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
newObservation <- exp_brut[exp_brut$event == "newObservation",]
dim(newObservation)
str(newObservation, max.level = 2)

bob <- newObservation[,5:6]


#### c'est assez hideux mais je fais vite

for(i in 1:length(newObservation)) {
    bob$point[i] <- st_sfc(st_point(newObservation$object[[i]]$location$coordinates))}

bob <- st_sf(bob, geom = bob$point) # la bonne colone pour le champs geom
bob <- bob[,-3]

class(bob)


carto_SE <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = bob[bob$username == "tjoliveau",], radius = 2, 
                     color = "red") 

carto_SE