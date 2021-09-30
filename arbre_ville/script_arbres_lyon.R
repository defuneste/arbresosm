### Script pour regarder comment améliorer les imports

library(sf)
library(dplyr)

arbres_lyon_final.shp <- st_read("abr_arbres_alignement.abrarbre.shp")
leaf <- read.csv2("leaft_type.csv", sep = "\t")

summary(leaf)

## regarder deciduous and evergreen, evergreen and decidous : meme chose

leaf$Genus %in% unique(arbres_lyon_final.shp$genus)  

unique(arbres_lyon_final.shp$genre)[unique(arbres_lyon_final.shp$genre) %in% leaf$Genus]   

unique(arbres_lyon_final.shp$genus)




## denotation = landmark, natural_monument, avenue, urban, cluster