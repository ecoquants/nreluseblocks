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
utm_zones_shp <- "/Volumes/Best HD/nrel_data_big/nga/UTM/MGRS_GZD_WorldWide/MGRS_GZD_WorldWide.shp"



# old ----

# 6x4 -> 2x1: 3x4
g_m <- st_make_grid(
  cellsize = c(2,1),
  offset = c(-180,-88), n = c(180,88*2), crs = st_crs(4326))
#g_h = data.frame(i = 1:length(g_h)) %>%
g_m = data.frame(m = rep(1:12, length.out=length(g_m))) %>%
  mutate(
    geom = g_m) %>%
  st_as_sf()
plot(g_m)
write_sf(g_m, "data/layers/blk/g_m.shp")



blk <- read_sf("data/layers/blk/usa_blk_u.shp")
blk <- blk %>%
  mutate(
    h = str_sub(PROT_NUMBE, 1, 1),
    z = str_sub(PROT_NUMBE, 2, 2),
    u = str_sub(PROT_NUMBE, 3, 4),
    m = str_sub(PROT_NUMBE, 6, 7) %>% as.integer())
head(blk)
blk_z <- blk %>%
  group_by(z) %>%
  summarize()
plot(blk_z)
write_sf(blk_z, "data/layers/blk/usa_blk_z.shp")
head(blk)

blk_m <- blk %>%
  group_by(m) %>%
  summarize()
write_sf(blk_m, "data/layers/blk/usa_blk_m.shp")
#plot(blk_m)

blk <- read_sf("data/layers/blk/usa_blk_u.shp")
blk <- blk %>%
  mutate(
    h = str_sub(PROT_NUMBE, 1, 1),
    x = str_sub(PROT_NUMBE, 2, 2),
    y = str_sub(PROT_NUMBE, 3, 4),
    m = str_sub(PROT_NUMBE, 6, 7) %>% as.integer())
head(blk)
blk_z <- blk %>%
  group_by(z) %>%
  summarize()
plot(blk_z)
write_sf(blk_z, "data/layers/blk/usa_blk_z.shp")




class(g_h)
attr(st_as_sf(g_h), "sf_column")
plot(g_h)
g_h <- st_make_grid(
  cellsize = c(360,90),
  offset = c(-180,-90), n = c(1,2), crs = st_crs(4326))
plot(g_h)

g_10d <- st_make_grid(
  cellsize = c(10,10),
  offset = c(-180,-90), n = c(36,18), crs = st_crs(4326))
plot(g_h)

cellsize = c(
  diff(st_bbox(x)[c(1, 3)]), diff(st_bbox(x)[c(2,
                                                          4)]))/n
g_h <- st_make_grid(NA_crs_, n=c(1,2))
plot(g_h)
m %>%
  mutate(
    n
  )

plot(m)
m2 <- st_segmentize(m, 4e5)
plot(m2)
st_make_grid()
