# Date: octobre 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: charger la base d'osm france postgres
# Description du problème:s
# La base est sur debian de l'université cf.
# maj_totale_france_journalisé.sh
# /!\ la deconnection n'est pas 
# Libraries utilisées:
# "RPostgreSQL"

# 1 Chargement des codes et de la library =======================
source("code.R")

library(RPostgreSQL) # fait le lien avec postgre, utilise DBI

# 2 Etablir une connexion ======================================

# charge les drivers pour postgre 
drv <- dbDriver("PostgreSQL")
# class(drv) #une verif

# fais un pont vers la db réutilisable
# ici j'ai pris une db en local pour tester
# con sera utilisé pour chaque connection et pkoi le franciser
con <- dbConnect(drv, dbname = dbname,
                 host = adresse , port = port, # attention 5432 par défaut
                 user = usr, password = pw) # idem pour user
rm(pw, dbname, adresse, port, usr) # mouais

print(dbListTables(con))

print("Il faut penser à se deconnceter! dbDisconnect(con)")