library(tidyverse)
library(glue)
library(here)
library(fs)
library(sf)
library(RPostgreSQL)
library(leaflet)
library(rpostgis)
library(raster)
library(rgdal)
#library(cleangeo) # devtools::install_github("eblondel/cleangeo")
library(lwgeom)
select = dplyr::select


source(here('scripts/con_pg_db.R'))

dbListTables(con)

# load vector
ak_shp <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Outer Continental Shelf Lease Blocks/blocks_dir/AK_BLKCLP.shp"
ak_ply <- read_sf(ak_shp)
ak_ply

shp <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Outer Continental Shelf Lease Blocks/PC_BLK_CLIP_dir/PC_BLK_CLIP.shp"
ply <- read_sf(shp) %>%
  st_transform(leaflet:::epsg4326) %>%
  mutate(
    BLOCK_NUMB = as.integer(BLOCK_NUMB),
    block_id = glue("{MMS_REGION}_{MMS_PLAN_A}_{PROT_NUMBE}_{BLOCK_NUMB}"))

#report <- clgeo_CollectionReport(ply)
#clgeo_SummaryReport(report)
#            type      valid         issue_type
#  rgeos_error:14   Mode :logical   NA's:14
#                   FALSE:14
#ply.clean <- clgeo_Clean(ply)
# ply <- st_make_valid(ply)
# idx_invalid <- which(!st_is_valid(ply))
# idx_invalid

st_write_db(conn, ply, table="boem_lease_blocks", geom_name="geom", drop=T, append=F) # dbRemoveTable(conn, "boem_lease_blocks")
dbExecute(conn, "CREATE INDEX idx_boem_lease_blocks_geom ON boem_lease_blocks USING GIST (geom);")
dbExecute(conn, "ALTER TABLE boem_lease_blocks ADD COLUMN pkey SERIAL PRIMARY KEY;")
#a <- dbGetQuery(conn, "SELECT pkey, ST_IsValidReason(geom) as validity_info FROM boem_lease_blocks WHERE ST_IsValid(geom) = false;")
#a
# TODO: sf_to_pg <- function(o_sf, conn, tbl_name)
# TODO: create / google for BOEM lease blocks in Great Lakes, Atlantic/Pacific Islands

ca_shp <- "~/Desktop/2018-07-17_desktop/desktop 2018-04-04/desktop 2018-02-23/_old_desktops/desktop_2016-04-11/desktop/esm296-4f/California_50m_wind/california_50m_wind.shp"


fc2pg <- function(ply, conn, table, append=F){
  # TODO: check out rpostgis::pgInsert() w/ fancier +gid col, writeWKB method

  #table = "wind_ca_50m"
  st_write_db(conn, ply, table, geom_name="geom", drop=ifelse(append, F, T), append=append) # dbRemoveTable(conn, "boem_lease_blocks")
  dbSendQuery(conn, glue("CREATE INDEX idx_{table}_geom ON {table} USING GIST (geom);"))
  dbSendQuery(conn, glue("ALTER TABLE {table} ADD COLUMN pkey SERIAL PRIMARY KEY;"))
  #a <- dbGetQuery(conn, "SELECT pkey, ST_IsValidReason(geom) as validity_info FROM {table} WHERE ST_IsValid(geom) = false;")
}
ply <- read_sf(ca_shp)
fc2pg(ply, conn, "wind_ca_50m")

# read vector
blks <- st_read(conn, "boem_lease_blocks")


# plot vector in leaflet
pal <- colorNumeric("Spectral", as.numeric(blks$BLOCK_NUMB))
leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addPolygons(
    data = blks,
    fillColor = ~pal(as.numeric(BLOCK_NUMB)), fillOpacity = 0.7,
    color = "white", opacity = 0.7)

# load raster
tif <- "~/github/nrel-uses/data/layers/depth/West_depth_epsg4326.tif"
r <- raster(tif)
r <- raster::shift(r, -360) # unwrap
plot(r)

#gdb <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/2013 Vessel Density- Tanker/VesselDensityTanker2013.gdb"
r_blocks <- c(ceiling(ncol(r)/50), ceiling(nrow(r)/50))
pgWriteRast(conn, "r_depth_west", r, bit.depth = NULL, blocks = r_blocks, constraints = T, overwrite = T)

r <- pgGetRast(conn, "r_depth_west")
plot(r)

# metadata
r_m <- dbGetQuery(conn, "SELECT (md).*, (bmd).* FROM (SELECT ST_Metadata(rast) AS md, ST_BandMetadata(rast) AS bmd  FROM r_depth_west LIMIT 1) foo;")
r_m

# raster blocks
r_b <- st_read_db(conn, query="SELECT rid, rast::geometry AS geom FROM r_depth_west;")
r_b; plot(r_b)

# raster extent
r_e <- st_read_db(conn, query="SELECT ST_Buffer(ST_Union(rast::geometry), 0.000001) FROM r_depth_west;")
r_e; plot(r_e)

#r_na <- dbGetQuery(conn, "SELECT ST_BandHasNodataValue(rast), ST_BandNodataValue(rast) FROM r_depth_west LIMIT 1;")
#r_na

ply
plot(ply)


sql <- readLines("https://raw.githubusercontent.com/pedrogit/postgisaddons/master/postgis_addons.sql")
dbExecute(conn, sql)
a <- dbGetQuery(
  conn,
  "SELECT pkey, (ST_AreaWeightedSummaryStats(gv)).*
  FROM (
    SELECT pkey, ST_Intersection(rast, geom) gv
    FROM r_depth_west, boem_lease_blocks
    WHERE ST_Intersects(rast, geom)) foo
  GROUP BY pkey;")
a

# https://raw.githubusercontent.com/pedrogit/postgisaddons/master/postgis_addons.sql

a <- dbGetQuery(
  conn,
  "SELECT pkey, (ST_SummaryStatsAgg(ST_Clip(rast, 1, geom, true))).*
  FROM r_depth_west, boem_lease_blocks
  WHERE ST_Intersects(rast, geom)
  GROUP BY pkey;")
a


"SELECT pkey, AVG((ST_SummaryStats(ST_CLIP(rast, 1, geom, true))).mean)
   FROM r_depth_west, boem_lease_blocks
   WHERE ST_Intersects(geom, rast)
   GROUP BY pkey;")


boem_lease_blocks_s001
CREATE TABLE x_s001 AS
SELECT foo.rid, foo.pkey, ST_AsText((foo.geomval).geom) As geomwkt, (foo.geomval).val
FROM (
  SELECT A.rid, g.pkey, ST_Intersection(A.rast, g.geom) As geomval
  FROM r_depth_west AS A, boem_lease_blocks_s001 AS g) As foo;



"-- SELECT ST_Intersection(r.rast, p.geom) AS gv FROM r_depth_west AS r, boem_lease_blocks AS p WHERE ST_Intersects(r.rast, p.geom);
DROP TABLE boem_lease_blocks_2;
CREATE TABLE boem_lease_blocks_2 AS
SELECT a.pkey, ST_Multi(ST_CollectionExtract(ST_DifferenceAgg(a.geom, b.geom), 3)) AS geom
FROM boem_lease_blocks a,
boem_lease_blocks b
WHERE ST_Equals(a.geom, b.geom) OR
((ST_Contains(a.geom, b.geom) OR
ST_Contains(b.geom, a.geom) OR
ST_Overlaps(a.geom, b.geom)) AND
(ST_Area(a.geom) < ST_Area(b.geom) OR
(ST_Area(a.geom) = ST_Area(b.geom) AND
ST_AsText(a.geom) < ST_AsText(b.geom))))
GROUP BY a.pkey;


CREATE INDEX idx_boem_lease_blocks_2_geom_gist
  ON boem_lease_blocks_2 USING gist(geom);

ALTER TABLE boem_lease_blocks_2 ADD COLUMN gid SERIAL PRIMARY KEY;

--To manually register this new table's geometry column in geometry_columns
-- Note that this approach will work for both PostGIS 2.0+ and PostGIS 1.4+
-- For PostGIS 2.0 it will also change the underlying structure of the table to
-- to make the column typmod based.
-- For PostGIS prior to 2.0, this technique can also be used to register views
SELECT populate_geometry_columns('boem_lease_blocks_2'::regclass);

CREATE TABLE x_2 AS SELECT ST_Intersection(r.rast, p.geom) AS gv FROM r_depth_west AS r, boem_lease_blocks_2 AS p WHERE ST_Intersects(r.rast, p.geom);

CREATE TABLE boem_lease_blocks_s001 AS
SELECT "OBJECTID","PROT_NUMBE","PROT_APRV_","BLOCK_NUMB","BLK_FED_AP","AREA_CODE","AC_LAB","BLOCK_LAB","MMS_REGION",
"MMS_PLAN_A","Shape_Leng","Shape_Area","block_id","pkey", ST_SimplifyPreserveTopology(geom, 0.001) AS geom
FROM boem_lease_blocks;

CREATE INDEX idx_boem_lease_blocks_s001_geom ON boem_lease_blocks_s001 USING GIST (geom);
SELECT populate_geometry_columns('boem_lease_blocks_s001'::regclass);
CREATE INDEX idx_boem_lease_blocks_s001_pkey ON boem_lease_blocks_s001 (pkey);
ALTER TABLE boem_lease_blocks_s001 ADD CONSTRAINT PK_boem_lease_blocks_s001 PRIMARY KEY USING INDEX idx_boem_lease_blocks_s001_pkey;




SELECT foo.rid, foo.pkey, ST_AsText((foo.geomval).geom) As geomwkt, (foo.geomval).val
FROM (
  SELECT A.rid, g.pkey, ST_Intersection(A.rast, g.geom) As geomval
  FROM r_depth_west AS A,

) As g(gid,geom)
WHERE A.rid = 2
) As foo;

"

a <- dbExecute(
  conn,
  "DROP TABLE x_boem_lease_blocks_x_depth_west;
  CREATE TABLE x_boem_lease_blocks_x_depth_west AS
    SELECT pkey, (gv).geom, (gv).val
    FROM (
      SELECT pkey, ST_Intersection(r.rast, p.geom) AS gv
      FROM
        r_depth_west AS r,
        (SELECT * FROM boem_lease_blocks WHERE ST_X(ST_Centroid(geom)) > -118) AS p
      WHERE ST_Intersects(r.rast, p.geom)) foo;")
#(SELECT * FROM boem_lease_blocks WHERE block_id NOT IN ('P_CEC_NJ10-08_6526')) AS p




#
# x <- st_read_db(
#   conn, query =
#     SELECT pkey, (gv).geom, (gv).val
#   FROM (
#     SELECT pkey, ST_Intersection(r.rast, p.geom) AS gv
#     FROM
#     r_depth_west AS r,
#     (SELECT * FROM boem_lease_blocks WHERE ST_X(ST_Centroid(geom)) > -118) AS p
#     WHERE ST_Intersects(r.rast, p.geom)) foo;
#
#     )


x <- st_read_db(conn, "x_boem_lease_blocks_x_depth_west")
x
plot(x)

a <- dbGetQuery(conn, "SELECT pkey, ST_X(ST_Centroid(geom)) AS x FROM boem_lease_blocks LIMIT 10")
a

#-122.70000002579995,37.533333379899922 -> block_id = 'P_CEC_NJ10-08_6526'
# Error in postgresqlExecStatement(conn, statement, ...) :
#   RS-DBI driver: (could not Retrieve the result : ERROR:  GEOSIntersects: TopologyException: side location conflict at -122.70000002579995 37.533333379899922
#                   CONTEXT:  PL/pgSQL function _st_intersects(geometry,raster,integer) line 22 at RETURN
#                   PL/pgSQL function st_intersection(geometry,raster,integer) line 5 at assignment
#                   SQL function "st_intersection" statement 1
#   )

a <- dbSendStatement(
  conn,
  "DROP TABLE x_wind_ca_50m_x_depth_west;
  CREATE TABLE x_wind_ca_50m_x_depth_west AS
    SELECT pkey, (gv).geom, (gv).val
    FROM (
      SELECT pkey, ST_Intersection(r.rast, p.geom) AS gv
      FROM
        r_depth_west AS r,
        wind_ca_50m AS p
    WHERE ST_Intersects(r.rast, p.geom)) foo;")

a <- dbSendStatement(conn,
                 "DROP TABLE x_wind_ca_50m_x_depth_west;
                  CREATE TABLE x_wind_ca_50m_x_depth_west AS
                 SELECT pkey, (gv).geom, (gv).val
                 FROM (
                 SELECT pkey, ST_Intersection(r.rast, p.geom) AS gv
                 FROM
                 r_depth_west AS r,
                 wind_ca_50m AS p
                 WHERE ST_Intersects(r.rast, p.geom)) foo;")
# NOTICE:  No pixels found for band 1
# CONTEXT:  SQL function "st_pixelaspolygons" statement 1
# SQL statement "SELECT public.ST_Collect(t.geom)            FROM public.ST_PixelAsPolygons(rast, nband) AS t"
# PL/pgSQL function _st_intersects(geometry,raster,integer) line 21 at SQL statement
# PL/pgSQL function st_intersection(geometry,raster,integer) line 5 at assignment
# SQL function "st_intersection" statement 1

# PREVIOUSLY
# Error in postgresqlExecStatement(conn, statement, ...) :
#   RS-DBI driver: (could not Retrieve the result : ERROR:  GEOSIntersects: TopologyException: side location conflict at -125.96666667939998 48.100000004299986
pgListGeom(conn)

x <- st_read_db(conn, "x_wind_ca_50m_x_depth_west")
x

a <- sf::st_read_db(
  conn, query =
    "SELECT pkey, ST_Intersection(rast, geom) AS geom
  FROM r_depth_west, boem_lease_blocks_2
  WHERE ST_Intersects(rast, ST_makeValid(geom))")
# NOTICE:  No pixels found for band 1
# CONTEXT:  SQL function "st_pixelaspolygons" statement 1
# SQL statement "SELECT public.ST_Collect(t.geom)            FROM public.ST_PixelAsPolygons(rast, nband) AS t"
# PL/pgSQL function _st_intersects(geometry,raster,integer) line 21 at SQL statement
# PL/pgSQL function st_intersection(geometry,raster,integer) line 5 at assignment
# SQL function "st_intersection" statement 1
# ... repeate 2x above...
# Error in postgresqlExecStatement(conn, statement, ...) :
#   RS-DBI driver: (could not Retrieve the result : ERROR:  GEOSIntersects: TopologyException: side location conflict at -123.75000002159996 32.166666734699888

r_ply <- sf::st_read_db(
  conn, query =
    "SELECT *, ST_PixelAsPolygons(rast, 1, ) AS geom
  FROM r_depth_west;")

a <- dbSendQuery(conn,
                 "CREATE TABLE x_boem_lease_blocks_x_depth_west AS
                 SELECT pkey, (gv).geom, (gv).val
                 FROM (
                 SELECT pkey, ST_Intersection(r.rast, p.geom) AS gv
                 FROM
                 r_depth_west AS r,
                 boem_lease_blocks AS p
                 WHERE ST_Intersects(r.rast, p.geom)) foo;")

# try switching around the arguments in ST_Intersects() from raster, geometry to geometry, raster
a <- st_read_db(
  conn, query =
  "SELECT pkey, ST_Intersection(geom, rast) AS geomval
   FROM boem_lease_blocks, r_depth_west
   WHERE ST_Intersects(geom, rast);")

"CREATE TABLE sum_pop3 AS
SELECT gid, SUM((ST_SummaryStats(ST_Clip(rast,1,geom))).sum)
FROM perez_grid, ls_den
WHERE ST_Intersects(geom,rast)
GROUP BY gid
"

# http://movingspatial.blogspot.com/2012/07/postgis-20-intersect-raster-and-polygon.html
system.time({ # 24 sec
a <- dbGetQuery(
  conn,
  "SELECT pkey, AVG((ST_SummaryStats(ST_CLIP(rast, 1, geom, true))).mean)
   FROM r_depth_west, boem_lease_blocks
   WHERE ST_Intersects(geom, rast)
   GROUP BY pkey;")
})

# push, add col, update with join
dbWriteTable(conn, "tmp", a)
dbSendQuery(conn, "ALTER TABLE boem_lease_blocks ADD COLUMN depth REAL;")
dbSendQuery(conn, "UPDATE boem_lease_blocks AS b SET depth = t.avg FROM tmp AS t WHERE t.pkey = b.pkey;")
# YAY! Works TODO: try Postgres 9.6+ so ST_Intersection(geom, rast) works

# [WKTRasterTutorial01 – PostGIS](https://trac.osgeo.org/postgis/wiki/WKTRasterTutorial01)
# [PostGIS — Getting intersections the faster way](https://postgis.net/2014/03/14/tip_intersection_faster/)
# http://www.postgis.us/downloads/postgis21_cheatsheet.pdf
# http://www.postgis.us/downloads/postgis21_raster_cheatsheet.pdf
# [PostGIS 2.0 - Intersect Raster and Polygon | Moving Spatial](http://movingspatial.blogspot.com/2012/07/postgis-20-intersect-raster-and-polygon.html)
# [Five Fast and Replicable Spatial Tasks with PostGIS « Rex Douglass](http://rexdouglass.com/postgis/)
# https://postgis.net/docs/RT_ST_Intersection.html
# a <- rpostgis::pgGetGeom(conn,
a <- st_read_db(
  conn, query =
  "SELECT
	foo.rid,
  foo.gid,
  ST_AsText((foo.geomval).geom) As geomwkt,
  (foo.geomval).val
  FROM (
  SELECT
  A.rid,
  g.gid,
  ST_Intersection(A.rast, g.geom) As geomval
  FROM dummy_rast AS A
  CROSS JOIN (
  VALUES
  (1, ST_Point(3427928, 5793243.85) ),
  (2, ST_GeomFromText('LINESTRING(3427927.85 5793243.75,3427927.8 5793243.75,3427927.8 5793243.8)')),
  (3, ST_GeomFromText('LINESTRING(1 2, 3 4)'))
  ) As g(gid,geom)
  WHERE A.rid = 2
  ) As foo;")

a <- dbSendQuery(conn,
                 "CREATE TABLE x_boem_lease_blocks_x_depth_west AS
                 SELECT p.*, (ST_Intersection(rast, geom)).geom AS geom2, (ST_Intersection(rast, geom)).val
                 FROM boem_lease_blocks AS p, r_depth_west AS r
                 WHERE ST_Intersects(rast, geom);")
# ERROR:  Raster and geometry do not have the same SRID

a <- dbGetQuery(conn, "SELECT ST_SRID(rast) As srid FROM r_depth_west WHERE rid=1;")
a

a <- dbGetQuery(conn, "SELECT ST_SRID(geom) As srid FROM boem_lease_blocks;")
a

a <- dbSendQuery(conn,
                 "SELECT  OBJECTID, (stats).*
  FROM (
    SELECT OBJECTID, ST_SummaryStats(ST_Clip(rast,2,geom)) As stats
    FROM r_depth_west AS r
      INNER JOIN boem_lease_blocks AS P
      ON ST_Intersects(p.geom, r.rast)) As foo;")
a

a <- dbGetQuery(conn,
                "SELECT p.*, ST_SummaryStats(ST_Clip(r.rast::rast, p.geom::geom)) As stats
    FROM r_depth_west AS r
      INNER JOIN boem_lease_blocks AS p
      ON ST_Intersects(p.geom, r.rast);")
a
