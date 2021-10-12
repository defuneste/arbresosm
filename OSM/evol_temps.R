# Date: octobre 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: nb d'arbre isolé en fonction du temps dans OSM
# Description du problème:
# Les CSV viennent d'une instance overpass en local 
#
# Libraries utilisées:
# sf

remotes::install_github("nowosad/spData")

# Il faut commencer par verifier les bbox associées à ces exports 
# pour verifier si elles se superposent 

library(sf)
library(spData)

#overpass bbox is S - W - N -E
# sf is xmin / xmax / ymax / ymin 
# xmin = W / X max = E / ymax = N / Y min = S

reorg_bbox <- function(x) {
    x <- c(xmin = x[2], xmax = x[4], ymin = x[3], ymax = x[1])
    return(x)
}


europe_op <- c(34,-13,65,48)
USA_op <- c(24,-126,51,-65)
Africa_op <- c(36,-21,39,55)
Asia_op <- c(11,25,77,180)
NAmerica_op <- c(9,-168,73,-51)
SAmerica_op<- c(-56,-90,17,-31)
Australia_op <- c(-47,106,8,180)

europe <- sf::st_as_sfc(sf::st_bbox(reorg_bbox(europe_op), 
                      crs = sf::st_crs(4326)))

usa <- sf::st_as_sfc(sf::st_bbox(c(xmin = -126, xmax = -65, ymax = 51, ymin = 24), 
                                 crs = sf::st_crs(4326)))

africa <- sf::st_as_sfc(sf::st_bbox(c(xmin = -126, xmax = -65, ymax = 51, ymin = 24), 
                                    crs = sf::st_crs(4326)))

plot(world$geom)
plot(europe, col = "red", add = T)
plot(usa, col = "red", add = T )



