## reprise du script des arbres de SE 
# correction des ws et autres erreurs

# package

library(sf)
library(stringr)
library(dplyr)

arbres_se.shp <- st_read("data/arbres_se_brut.geojson")
summary(arbres_se.shp) # à virer

# les infos sont dans une longue liste de Mots dans description
#############################################################################################################################################
# le genre
# ici j'ai supprimé les x 
arbres_se.shp$DESCRIPTION[arbres_se.shp$genus == "x"] <- str_remove(arbres_se.shp$DESCRIPTION[arbres_se.shp$genus == "x"],  "\\s|[x]\\s")

arbres_se.shp$genus <- word(arbres_se.shp$DESCRIPTION, 1) # ici on peut prendre le premier mot et assumer le genre, genus dans OSM
sort(unique(arbres_se.shp$genus), na.last = F)

#### Pb lié à "indéterminé"
# soliution les virer car probablement pas des arbres presents
# "Emplacement d'arbres Indéterminé Indéterminé Emplacement Vide"

arbres_se.shp <- arbres_se.shp[!arbres_se.shp$genus == "Indéterminé", ] 

#### pb lié à inconnu
# pas evident de savoir à quoi correspond 
# solution pas garder et faire un fichier a part avec les "inconnu"  et indeterminé

arbres_se.shp <- arbres_se.shp[!arbres_se.shp$genus == "inconnu", ] 

##########################################################################################################################################
# l'éspèce

# on combien les deux premiers mots
arbres_se.shp$species <- paste(word(arbres_se.shp$DESCRIPTION, 1), 
                               word(arbres_se.shp$DESCRIPTION, 2)) # initialisation

# le sp. est unitile le champ genre est suffisant
arbres_se.shp$species[word(arbres_se.shp$DESCRIPTION, 2) == "sp."] <- NA # sp en NA
arbres_se.shp$species[word(arbres_se.shp$DESCRIPTION, 2) == "inconnu"] <- NA # inconnu en NA

# pour "x" on va prendre les trois premiers mots pour constituer l'hybride, c'est long car il y a pas mal d'indexation
# on indexe par rapport à "x"
arbres_se.shp$species[word(arbres_se.shp$DESCRIPTION, 2) == "x"] <- paste(word(arbres_se.shp$DESCRIPTION, 1)[word(arbres_se.shp$DESCRIPTION, 2) == "x"],
                                                                          word(arbres_se.shp$DESCRIPTION, 2)[word(arbres_se.shp$DESCRIPTION, 2) == "x"], 
                                                                          word(arbres_se.shp$DESCRIPTION, 3)[word(arbres_se.shp$DESCRIPTION, 2) == "x"])
# on a deux cas qui ne sont pas passé à cause de whitespace
arbres_se.shp$species[word(arbres_se.shp$DESCRIPTION, 2) == ""] <- paste(word(arbres_se.shp$DESCRIPTION, 1)[word(arbres_se.shp$DESCRIPTION, 2) == ""],
                                                                         word(arbres_se.shp$DESCRIPTION, 3)[word(arbres_se.shp$DESCRIPTION, 2) == ""])

###############################################################################################################################################
# nom vernaculaire

arbres_se.shp <- arbres_se.shp %>% 
    # ici le "species:fr" pourrait être un "taxon:fr" car on pas mal de nom de cultivar
    # on extrait tout ce qui correspond au patron, le str_extract est plus cours qu'une indexation avec un grep
    # par contre il ne retourne que le premier cas, si il n'y en a plusieurs ce qui ne devrait pas être notres cas
    mutate("species:fr" = str_extract(arbres_se.shp$DESCRIPTION, pattern = "(?<=\\:\\s).*"))

## il y a un truc sur vide et occupée je met de coté ici mais pe à regarder si on fait "ceci n'est pas un arbre"


################################################################################################################################################
# mise en forme OSM

arbres_se.final.shp <- arbres_se.shp %>% 
    rename("ref:FR:Saint-Etienne:tree" = GID) %>% # on garde l'id de la BD
    mutate("source:name" = "Arbres d'alignement de la ville de Saint-Etienne", #source des données
           denotation = "avenue") %>% # arbre d'avenue, c'est pas tjrs le cas 
    select(-APIC_CDATE, -APIC_MDATE, -NATURE, -CD_LIMCO, -RIVOLI,  -DESCRIPTION) # on enleve les champs non utiles dans OSM
summary(arbres_se.final.shp) # une verif

#################################################################################################################################################
# rajout leaf_cycle et leaf_type
# le .csv vient de la liste OSM que j'ai faite 

leaf2 <- read.csv2("data/leaf_type_ajout.csv", sep = ",", stringsAsFactors = FALSE) 
   
leaf2 <- leaf2 %>% mutate(Genus = trimws(str_trim(leaf2$Genus))) 
    

arbres_se.final.shp <-  left_join(arbres_se.final.shp, leaf2, by = c("genus" = "Genus")) %>% 
                            st_transform(4326)

arbres_se.final.shp$leaf_cycle <- trimws(arbres_se.final.shp$leaf_cycle)
arbres_se.final.shp$leaf_type <- trimws(arbres_se.final.shp$leaf_type)

st_write(arbres_se.final.shp, dsn = "data/arbres_se_final2021.geojson")