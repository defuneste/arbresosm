# Date: octobre 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: cartes des arbres isolés dans osm mondiale
# Description du problème:
# Les CSV viennent d'une instance overpass en local 

# Libraries utilisées:
# sf, spData

# Il faut commencer par verifier les bbox associées à ces exports 
# pour verifier si elles se superposent 


library(sf)
library(spData)


liste_csv <- list.files("data/coord/", 
                        full.names = TRUE)

bbox <- lapply(liste_csv, 
               read.csv, 
               quote = "", sep = "\t", col.names = c("id", "lat", "lon"))

bbox <- do.call("rbind", bbox)

library(RPostgreSQL)

source("code2.R")

drv <- dbDriver("PostgreSQL")

con <- dbConnect(
    drv,
    user = usr,
    password = pwd,
    dbname = "arbremonde",
    host = "localhost",

)

dbListTables(con)

bbox_head <- bbox[1:100,]

write.csv2(bbox, "data/arbre_monde2.csv")

bbox.shp <- st_as_sf(bbox,
                     coords = c("lon", "lat"),
                     crs = 4326)

st_is_longlat(bbox.shp)

plot(world$geom)

# au final geom_bind2d utilise x = lon et y = lat
# si on veut changer la proj et rester avec bind2e il faudra reprojeter


projected_Eckert  <- st_crs("+proj=eck4")
bbox_Eckert  <- st_transform(bbox.shp, crs = projected_Eckert )

world_robin <- st_transform(world, crs = projected_robin)

arbre_plot <- world_robin %>% 
    ggplot() +
    geom_bin2d(data = bbox, 
               aes(x = lon, 
                   y = lat, 
                   fill = after_stat(log10(count))), 
               binwidth = 1) +
    geom_sf(color = "white", size = 0.1, fill = "transparent") +
    theme_bw() +
    scale_fill_viridis_c(option = "inferno") +
    theme(panel.background = element_rect(fill = "black"))

arbre_plot + 
    geom_sf(color = "white", size = 0.1, fill = "transparent") +
    theme(panel.background = element_rect(fill = "black"))
    
