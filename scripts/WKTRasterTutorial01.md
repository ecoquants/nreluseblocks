
```
docker exec -it nrel-postgis bash
cd /data; mkdir WKTRasterTutorial01
```


```
docker cp ~/github/nrel-uses/data/layers/depth/West_depth_epsg4326.tif nrel-postgis:/data/WKTRasterTutorial01/West_depth_epsg4326.tif
```

shp2pgsql
