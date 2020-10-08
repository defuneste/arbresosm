#! /bin/bash 

# Mars 2019 R Mayoud, O leroy
# script mise à jour de la bdd France OSM à partir de datagouv : http://download.openstreetmap.fr/extracts/europe/france-latest.osm.pbf
# attention il y a deux communes suisses + Geneve 

# définition des variables

ETENDUE=champagne-ardenne-latest
DB=osmdbchampagne
JOURNAL=rapportjournal.txt

# creation du journal

date > /osm_maj_auto/osm_dl/$JOURNAL
echo "" >> /osm_maj_auto/osm_dl/$JOURNAL
date
echo ""

# suppression du pbf n-2

if [ -f /osm_maj_auto/osm_dl/$ETENDUE-old.osm.pbf ]
then
rm -fv /osm_maj_auto/osm_dl/$ETENDUE-old.osm.pbf >> /osm_maj_auto/osm_dl/$JOURNAL
echo "" >> /osm_maj_auto/osm_dl/$JOURNAL
echo "rm -fv /osm_maj_auto/osm_dl/$ETENDUE-old.osm.pbf" 
echo "" 
fi

# renommage de l'ancien pbf (n-1) si il existe

if [ -f /osm_maj_auto/osm_dl/$ETENDUE.osm.pbf ]
then
mv -v /osm_maj_auto/osm_dl/$ETENDUE.osm.pbf /osm_maj_auto/osm_dl/$ETENDUE-old.osm.pbf >> /osm_maj_auto/osm_dl/$JOURNAL
echo "" >> /osm_maj_auto/osm_dl/$JOURNAL
echo "mv -v /osm_maj_auto/osm_dl/$ETENDUE.osm.pbf /osm_maj_auto/osm_dl/$ETENDUE-old.osm.pbf"
echo "" 
fi



# dl du pbf n 

# wget http://download.openstreetmap.fr/extracts/europe/france-latest.osm.pbf /osm_maj_auto/osm_dl

echo "Debut wget"
echo "Debut wget" >> /osm_maj_auto/osm_dl/$JOURNAL
wget -nv http://download.geofabrik.de/europe/france/$ETENDUE.osm.pbf -O /osm_maj_auto/osm_dl/$ETENDUE.osm.pbf  -a /osm_maj_auto/osm_dl/$JOURNAL
echo "Fin wget"
echo ""
echo "Fin wget" >> /osm_maj_auto/osm_dl/$JOURNAL
echo "" >> /osm_maj_auto/osm_dl/$JOURNAL



# on supprime la db n-1
# un fichier (.pgpass) a été crée dans root avec *:*:*:postgres:[pass]
echo "Debut commandes psql"
echo "Debut commandes psql" >> /osm_maj_auto/osm_dl/$JOURNAL
psql -U postgres -c "DROP DATABASE $DB;" -w >> /osm_maj_auto/osm_dl/$JOURNAL


# on crée la db et les extensions
psql -U postgres -c "CREATE DATABASE $DB;" -w >> /osm_maj_auto/osm_dl/$JOURNAL
psql -U postgres -d $DB -c 'CREATE EXTENSION postgis;' -w >> /osm_maj_auto/osm_dl/$JOURNAL
psql -U postgres -d $DB -c 'CREATE EXTENSION postgis_topology;' -w >> /osm_maj_auto/osm_dl/$JOURNAL
psql -U postgres -d $DB -c 'CREATE EXTENSION hstore;' -w >> /osm_maj_auto/osm_dl/$JOURNAL

echo "Fin commandes psql"
echo ""
echo "Fin commandes psql" >> /osm_maj_auto/osm_dl/$JOURNAL
echo "" >> /osm_maj_auto/osm_dl/$JOURNAL


# il faut osm2pgsql https://wiki.openstreetmap.org/wiki/Osm2pgsql
echo "Debut osm2pgsql"
echo "Debut osm2pgsql" >> /osm_maj_auto/osm_dl/$JOURNAL
osm2pgsql -v -c -d $DB --slim --cache 8000 --number-processes 2 --hstore --extra-attributes --multi-geometry /osm_maj_auto/osm_dl/$ETENDUE.osm.pbf -H localhost -U postgres >> /osm_maj_auto/osm_dl/$JOURNAL 2>&1 # ici on demande de mettre "les erreurs"  dans le journal mais ce sont plus des affichages ecran traités comme des erreurs

echo "Fin osm2pgsql"
echo ""
echo "Fin osm2pgsql" >> /osm_maj_auto/osm_dl/$JOURNAL
echo "" >> /osm_maj_auto/osm_dl/$JOURNAL

#verification des tables dans la db 

echo "Les tables publiques présentes :" 
psql -U postgres  -d $DB -c "SELECT table_name FROM information_schema.tables WHERE table_schema='public';"
echo ""
echo "Les tables publiques présentes :" >> /osm_maj_auto/osm_dl/$JOURNAL
psql -U postgres  -d $DB -c "SELECT table_name FROM information_schema.tables WHERE table_schema='public';" >> /osm_maj_auto/osm_dl/$JOURNAL
echo ""

# Affichage heure de fin
date
date >> /osm_maj_auto/osm_dl/$JOURNAL
echo "" >> /osm_maj_auto/osm_dl/$JOURNAL

# echo "" >> /osm_maj_auto/osm_dl/$JOURNAL
# echo "FIN SCRIPT" >> /osm_maj_auto/osm_dl/$JOURNAL



