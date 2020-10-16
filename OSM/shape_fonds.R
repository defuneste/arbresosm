### projet pour l'analyse des arbres seul dans OSM 
# ici l'objectif est plus de prendre des fonds pour delimiter des zones avec les arbres
# octobre 2020
# Petites notes sur le code :
# pour me rapeller qu'un objet à des infos geometriques type vecteur je lui accolle un .shp
# si c'est un raster .grid, si c'est un dataframe souvent un .dat

##.###################################################################################33
## I Chargement des différents packages demandés et données ====
##.#################################################################################33

pkgs <-  c("sf", "rnaturalearth", "dplyr")
inst <- lapply(pkgs, library, character.only = TRUE)

# str(ne_countries(returnclass='sf'))

# countries110 <- ne_download(scale = 110, type = 'countries')
# sp::plot(countries110)


continent10.shp <-st_read("data/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

continent.shp <- continent10.shp  %>% 
    dplyr::select(CONTINENT) %>% 
    group_by(CONTINENT) %>% summarise()


rm(countries)
# 
# plot(continent.shp)