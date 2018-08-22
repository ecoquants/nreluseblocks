# libraries
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

# paths ----
eez_gcs_geo    <- file.path(here("../nrel-uses/data/layers/eez/usa_ter_gcs.geojson"))
eez_wgcs_geo   <- file.path(here("../nrel-uses/data/layers/eez/usa_ter_wgcs.geojson"))
eez_u_shp      <- file.path(here("data/layers/eez/usa_ter_u.shp"))
blk_u_shp      <- file.path(here("data/layers/blk/usa_blk_u.shp"))
blk_todo_shp      <- file.path(here("data/layers/blk/usa_blk_todo.shp"))
utm_zones_shp  <- "/Volumes/Best HD/nrel_data_big/nga/UTM/UTM_Zone_Boundaries/UTM_Zone_Boundaries.shp"

# functions ----
sf_read <- function(x, crs=4326, ...){
  # read_sf(), except ensure geometry column is named geom
  # x <- eez_gcs_geo

  o <- read_sf(x)
  o_geom <- attr(o, "sf_column")
  q_geom <- quo(o_geom)

  if (o_geom != "geom"){
    stopifnot(!"geom" %in% names(o)) # stop if already exists
    o <- rename(o, geom = !! q_geom)
  }

  # TODO: st_crs(4326)
  if (st_crs(o) == NA_crs_){
    # TODO: ok assume 4326?
    #browser()
    st_crs(o) <- crs
  } else if (!st_crs(o) == st_crs(crs)){
    #browser()
    # TODO: ok to project to crs?
    o <- st_transform(o, crs)
  }
  o
}
sf_erase = function(x, y){
  st_difference(x, st_union(st_combine(y))) }
sf_intersects = function(a, b){
  i <- sf::st_intersects(a, b)
  dplyr::slice(a, which(sapply(i, length) > 0))
}

# read in shapefiles
eez_gcs_sf   <- sf_read(eez_gcs_geo)
utm_zones_sf <- sf_read(utm_zones_shp)

# extract < [-180,180]
eez_ok <- st_intersection(eez_gcs_sf, st_union(st_combine(utm_zones_sf)))
#plot(eez_ok["territory"])

# extract < [-360,-180]
eez_left <- sf_erase(eez_gcs_sf, utm_zones_sf)

# convert < [-360,-180] to [0,180]
eez_right <- eez_left %>%
  mutate(
    geom = (geom + c(360,90)) %% c(360) - c(0,90)) %>%
  st_set_geometry("geom") %>%
  st_set_crs(4326)
#plot(eez_right["territory"])

# union ok and right
eez_u <- rbind(eez_ok, eez_right) %>%
  group_by(territory, area_km2) %>%
  summarise() %>%
  st_cast()

# eez

# show table
# eez_u %>%
#   st_set_geometry(NULL)

# plot map
# pal <- colorFactor(rainbow(8), eez_u$territory)
# leaflet(eez_u) %>%
#   addProviderTiles(providers$Esri.OceanBasemap) %>%
#   addPolygons(color = ~pal(territory))

# write to file sys
write_sf(eez_u, eez_u_shp)

eez_u <- read_sf(eez_u_shp)

dir_blk <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Outer Continental Shelf Lease Blocks"
atl_shp <- file.path(dir_blk, "ATL_BLKCLIP_dir/ATL_BLKCLP.shp")
gom_shp <- file.path(dir_blk, "blk_clip_dir/blk_clip.shp")
ak_shp  <- file.path(dir_blk, "blocks_dir/AK_BLKCLP.shp")
pac_shp <- file.path(dir_blk, "PC_BLK_CLIP_dir/PC_BLK_CLIP.shp")

blk_u <- rbind(
  sf_read(atl_shp), # 4269
  sf_read(gom_shp) %>%
    select(PROT_NUMBE, PROT_APRV_, BLOCK_NUMB, BLK_FED_AP, MMS_REGION, MMS_PLAN_A), # 4267
  sf_read(ak_shp) %>%
    select(PROT_NUMBE, PROT_APRV_, BLOCK_NUMB, BLK_FED_AP, MMS_REGION, MMS_PLAN_A),  # 4269
  sf_read(pac_shp) %>%
    select(PROT_NUMBE, PROT_APRV_, BLOCK_NUMB, BLK_FED_AP, MMS_REGION, MMS_PLAN_A)) # 4269

write_sf(blk_u, blk_u_shp)
blk_u <- read_sf(blk_u_shp)

blk_todo <- sf_erase(eez_u, blk_u)
write_sf(blk_todo, blk_todo_shp)

# TODO: clip blocks outside eez
blk_ui <- st_intersection(blk_u, eez_u)

# g_h: (h)emisphere, N or South ----
g_h <- st_make_grid(
  cellsize = c(360,90),
  offset = c(-180,-90), n = c(1,2), crs = st_crs(4326))
#g_h = data.frame(i = 1:length(g_h)) %>%
g_h = data.frame(h = c("S","N")) %>%
  mutate(
    geom = g_h) %>%
  st_as_sf()
plot(g_h)

# g_y: LETTERS ----
# 24-28 (G), 28 - 32 (H), 36-40° (J)
1:6*4
# A: Louisiana, X: Texas
lttrs <- LETTERS[1:((180/4-1)/2)]
g_y <- st_make_grid(
  cellsize = c(360,4),
  offset = c(-180,-88), n = c(1, 180/4-1), crs = st_crs(4326))
g_y = data.frame(y = c(rev(lttrs), lttrs)) %>%
  mutate(
    geom = g_y) %>%
  st_as_sf()
plot(g_y)
write_sf(g_y, "data/layers/blk/g_y.shp")

# g_x: UTM zone ----
g_x <- st_make_grid(
  cellsize = c(6,180),
  offset = c(-180,-90), n = c(60,1), crs = st_crs(4326))
g_x = data.frame(x = 1:60) %>%
  mutate(
    geom = g_x) %>%
  st_as_sf()
plot(g_x)

# g_hyx: intersection
g_hyx <- st_intersection(g_h, g_y) %>%
  st_intersection(g_x)

g_hyx <- g_hyx %>%
  mutate(
    geom_type = st_geometry_type(geom) %>% as.character()) %>%
  filter(geom_type == "POLYGON") %>%
  select(-geom_type)

g_hyx <- g_hyx %>%
  sf_intersects(eez_u)
write_sf(g_hyx, "data/layers/blk/g_hyx.shp")
#plot(g_hyxe["y"])

g_hyx <- sf_read("data/layers/blk/g_hyx.shp")

# get Alaska
i <- sf::st_intersects(g_hyx, eez_u, sparse = F)
i_ak <- which(i[,eez_u$territory == "Alaska"])
i_notak <- setdiff(1:nrow(g_hyx), i_ak)

#plot(g_hyx[i_ak,"x"])
#plot(g_hyx[setdiff(1:nrow(g_hyx), i_ak),"x"])

# g_*a: map 1 of 8 for Alaska  ----
g_hyxa <- g_hyx %>%
  slice(i_ak) %>%
  mutate(
    row = row_number(),
    a = NA)
m <- matrix(1:(2*4), nrow=4, ncol=2, byrow=T)
v <- apply(m, 1, rev) %>% rev() %>% as.vector()
for (i in 1:max(g_hyxa$row)){ # i <- 1
  g_i <- g_hyxa %>%
    filter(row == i)

  bb <- st_bbox(g_i)
  g_im <- st_make_grid(
    cellsize = c(3,1),
    offset = c(bb$xmin, bb$ymin), n = c(2,4), crs = st_crs(4326))
  d_i <- st_set_geometry(g_i, NULL)
  g_im <- d_i[rep(seq_len(nrow(d_i)), each=2*4),] %>%
    mutate(
      geom = g_im,
      a    = v) %>%
    st_as_sf()
  #plot(g_im['m'])

  cat(glue("{i}: {nrow(g_hyxa)} - 1 + {nrow(g_im)} = ..."), "\n")
  g_hyxa <- g_hyxa %>%
    filter(row != i) %>%
    rbind(
      g_im)
  cat(glue("{i}: ... = {nrow(g_hyxa)}"), "\n")
}
# plot(g_hyx['y'])
# plot(g_hyxa['m'])

g_hyxa <- g_hyxa %>%
  sf_intersects(eez_u)
write_sf(g_hyxa, "data/layers/blk/g_hyxa.shp")
g_hyxa <- sf_read("data/layers/blk/g_hyxa.shp")

# g_*m: map 1 of 12, except Alaska ----
g_hyxm <- g_hyx %>%
  slice(i_notak) %>%
  mutate(
    row = row_number(),
    m = NA)
m <- matrix(1:(3*4), nrow=4, ncol=3, byrow=T)
v <- apply(m, 1, rev) %>% rev() %>% as.vector()

for (i in 1:max(g_hyxm$row)){ # i <- 1
  g_i <- g_hyxm %>%
    filter(row == i)

  bb <- st_bbox(g_i)
  g_im <- st_make_grid(
    cellsize = c(2,1),
    offset = c(bb$xmin, bb$ymin), n = c(3,4), crs = st_crs(4326))
  d_i <- st_set_geometry(g_i, NULL)
  g_im <- d_i[rep(seq_len(nrow(d_i)), each=3*4),] %>%
    mutate(
      geom = g_im,
      m    = v) %>%
    st_as_sf()
  #plot(g_im['m'])

  cat(glue("{i}: {nrow(g_hyxm)} - 1 + {nrow(g_im)} = ..."), "\n")
  g_hyxm <- g_hyxm %>%
    filter(row != i) %>%
    rbind(
      g_im)
  cat(glue("{i}: ... = {nrow(g_hyxm)}"), "\n")
}
# plot(g_hyxe['y'])
# plot(g_hyxem['m'])

g_hyxm <- g_hyxm %>%
  sf_intersects(eez_u)
write_sf(g_hyxm, "data/layers/blk/g_hyxm.shp")

g_hyxa <- sf_read("data/layers/blk/g_hyxa.shp")
g_hyxm <- sf_read("data/layers/blk/g_hyxm.shp")

# g_hyxa + g_hyxm -> g_hyxc: combine Alaska (3deg) and not Alaska (2deg) cells
g_hyxc <- g_hyxa %>%
  mutate(m = NA) %>%
  rbind(
    g_hyxm %>%
      mutate(a = NA))
write_sf(g_hyxc, "data/layers/blk/g_hyxc.shp")
g_hyxc <- sf_read("data/layers/blk/g_hyxc.shp")

# write to db ----
write_sf(g_hyxc, dsn=con, layer="g_hyxc", overwrite=T)
#g_hyxc <- read_sf(dsn=con, layer="g_hyxc", geometry_column="geom")
#g_hyxc <- read_sf(dsn=con, layer="g_hyxc", geometry_column="geom")
# Error in st_sf(tbl, ...) : no simple features geometry column present

# blocks: 3 nmi^2; 1 nmi = 1 min lat = 1/60 dd
g_hyxcb <- g_hyxc %>%
  mutate(
    row = row_number())
write_sf(g_hyxcb, dsn=con, layer="g_hyxcb", overwrite=T)
dbSendQuery(con, "ALTER TABLE g_hyxcb ADD COLUMN b SMALLINT;")
dbSendQuery(con, "CREATE INDEX ON g_hyxcb (row);")

m <- t(sapply(seq(0, 50*19, by=50), function(x) x + 1:40 + 6000))
v <- apply(m, 1, rev) %>% rev() %>% as.vector() %>% as.integer()

#mapview::mapview(g_hyxemeb %>% filter(row == 789) + eez_u

t0 <- Sys.time()
nr <- max(g_hyxcb$row)
for (i in 1:nr){ # i <- 786
  # g_i <- g_hyxcb %>%
  #   filter(row == i)
  #bb <- st_bbox(g_i)
  #d_i <- st_set_geometry(g_i, NULL)
  ti <- Sys.time()

  d_i <- dbGetQuery(con, glue_sql("SELECT h,y,x,row,m,b FROM g_hyxcb WHERE row = {i};"))
  suppressWarnings({
    bb <- dbGetQuery(con, glue_sql("SELECT ST_Extent(geom) AS bbox FROM g_hyxcb WHERE row = {i};")) %>%
      mutate(
        bbox = str_replace_all(bbox, "[^[0-9] ,-]", "")) %>%
      separate(
        bbox, sep = "[, ]", convert = T,
        into = c("xmin", "ymin", "xmax", "ymax"))
  })
  g_ib <- st_make_grid(
    cellsize = c(3/60, 3/60),
    offset = c(bb$xmin, bb$ymin), n = c(2*20, 20), crs = st_crs(4326))
  g_ib <- d_i[rep(1, each=2*20*20),] %>%
    mutate(
      geom = g_ib,
      b    = v) %>%
    st_as_sf() %>%
    as("Spatial")
  #plot(g_ib['b'])

  dbSendQuery(con, glue_sql("DELETE FROM g_hyxcb WHERE row = {i};"))
  suppressMessages({
    r <- pgInsert(con, "g_hyxcb", g_ib)
  })

  #if (i %% 10 == 0){
    tn <- Sys.time()
    eta <- tn + ((nr-i) * (tn - t0) / (i))
    cat(sprintf("%04d of %d: %0.4f secs, eta %s\n", i, nr, difftime(tn, ti, units = "secs"), eta))
  #}
}
# TODO: index g_hyxcb gist in pg db, intersect with eez

# TODO: update Alaska with g_*x every 3 (vs 2) degrees
f

# plot(g_hyxem['m'])
# plot(g_hyxemb['m'])
#write_sf(g_hyxemeb, "data/layers/blk/g_hyxemeb.shp")
#g_hyxcb <- read_sf(con, "g_hyxcb")
g_hyxcb <- rpostgis::pgGetGeom(con, "g_hyxcb")

g_hyxcb %>%
  as("sf") %>%
  write_sf("data/layers/blk/g_hycb.shp")
  sf::as_Spatial()

# write to db ----
write_sf(eez_gcs_sf, dsn=con, layer="eez_gcs", overwrite=T)
write_sf(utm_zones_sf, dsn=con, layer="utm_zones", overwrite=T)

# intersections in postgis ----
q <- glue_sql(
  read_lines(here("scripts/x.sql")) %>% paste(collapse="\n"),
  A =   "eez_gcs", A_ID = "territory",
  B = "utm_zones", B_ID = "Zone_Hemi",
  O = "x_eez_utm", .con = con)
r <- dbGetQuery(con, q)
r

q <- "CREATE TABLE {`O`} AS
  SELECT a.{`A_ID`}, b.{`B_ID`}, ST_Intersection(a.geom, b.geom)
  FROM {`A`} AS a
  INNER JOIN {`B`} AS b
  ON (ST_Intersects(a.geom, b.geom)
    AND NOT ST_Touches(a.geom, b.geom) );


# make grids
n17


