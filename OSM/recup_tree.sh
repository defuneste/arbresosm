#! /bin/bash 

# octobre 2020
# extraction de natural=tree à partir de .pbf
# ajout dans une BD ici en local
# besoin de gdal et osmosis 

# le fichier configini est changé le modifier au besoin
export OSM_CONFIG_FILE=/home/lo82302h/Téléchargements/osm/custom_osmconf.ini 

for filename in *.pbf
 do
 	echo "Processing ${filename}."
    region=${filename}

    osmosis --read-pbf ${filename} --tf accept-nodes natural=tree --tf reject-ways --tf reject-relations --write-xml $region-tree.osm 

    ogr2ogr -f GeoJSON $region-tree.geojson $region-tree.osm points

    ogr2ogr -append -f "PostgreSQL" PG:"dbname=arbremonde user=postgres port=XXXX host=localhost, password=XXXX" $region-tree.geojson -progress 

done