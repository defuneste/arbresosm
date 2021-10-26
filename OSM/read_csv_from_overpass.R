# Date: octobre 2021
# Auteur: Olivier Leroy  www.branchtwigleaf.com/
# Objectif: lire les csv et au besoin les rexeporter pour en analyser les données
# Description du problème:
# Les CSV viennent d'une instance overpass en local. 
# Ils sont en general produit sur de multiple bbox qu'il faut réassembler


# 1- Lecture des csv ===========================================================
# Dans un premier temps on  va lire les csv puis on va les ajouter lignes par lignes

liste_csv <- list.files("data/coord/", 
                        full.names = TRUE)

bbox <- lapply(liste_csv, 
                read.table, 
                quote = "", 
                sep = "\t", 
                header = TRUE,
                # Je trouve mieux de specifier les colonnes et leur data types
                col.names = c("id", "lat", "lon"), 
                colClasses = c("character", "numeric", "numeric"))

bbox <- do.call("rbind", bbox)


# 2- Dans un second temps on ecris un
write.table(bbox2, 
            "data/arbre_monde.csv",
            col.names = FALSE,
            row.names = FALSE,
            # point important afin d'evite que postgre rajoute des "
            quote = FALSE, 
            sep = ",")

