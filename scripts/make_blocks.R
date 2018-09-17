# libraries ----
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
eez_gcs_geo    <- here("../nrel-uses/data/layers/eez/usa_ter_gcs.geojson")
eez_wgcs_geo   <- here("../nrel-uses/data/layers/eez/usa_ter_wgcs.geojson")
eez_u_shp      <- here("data/layers/eez/usa_ter_u.shp")
blk_shp        <- here("data/layers/blk/usa_blk.shp")
blk_eez_shp    <- here("data/layers/blk/usa_blk_eez.shp")
blk_todo_shp   <- here("data/layers/blk/usa_blk_todo.shp")
blk_plan_shp   <- here("data/layers/blk/usa_blk_plan.shp")
utm_zones_shp  <- "/Volumes/Best HD/nrel_data_big/nga/UTM/UTM_Zone_Boundaries/UTM_Zone_Boundaries.shp"
g_hyx_shp      <- here("data/layers/blk/g_hyx.shp")
g_hyxa_shp     <- here("data/layers/blk/g_hyxa.shp") # Alaska map areas
g_hyxm_shp     <- here("data/layers/blk/g_hyxm.shp") # map areas, not Alaska
g_hyxc_shp     <- here("data/layers/blk/g_hyxc.shp") # map areas, combined (Alaska + not)

dir_blk <- "/Volumes/Best HD/nrel_data_big/marinecadastre.gov/Outer Continental Shelf Lease Blocks"
atl_shp <- file.path(dir_blk, "ATL_BLKCLIP_dir/ATL_BLKCLP.shp")
gom_shp <- file.path(dir_blk, "blk_clip_dir/blk_clip.shp")
ak_shp  <- file.path(dir_blk, "blocks_dir/AK_BLKCLP.shp")
pac_shp <- file.path(dir_blk, "PC_BLK_CLIP_dir/PC_BLK_CLIP.shp")

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

sf_erase <- function(x, y){
  st_difference(x, st_union(st_combine(y))) }

sf_intersects <- function(a, b){
  i <- sf::st_intersects(a, b)
  dplyr::slice(a, which(sapply(i, length) > 0))
}

sf_box <- function(lng1, lat1, lng2, lat2, box_val = "testNW"){
  # return spatial features box

  ll <- list(x = lng1, y = lat1)
  ur <- list(x = lng2, y = lat2)
  geometry = st_sfc(st_polygon(list(cbind(
    c(ll$x, ur$x, ur$x, ll$x, ll$x),
    c(ll$y, ll$y, ur$y, ur$y, ll$y)))))
  b = st_sf(box = box_val, geometry, crs=leaflet:::epsg4326)
}

pg_intersection <- function(con, x, y, z){
  # https://postgis.net/2014/03/14/tip_intersection_faster/
  # x = "g_hyxcb"
  # y = "testNW"
  # z = "g_hyxcb_nw"

  #dbSendQuery(con, "ALTER TABLE eez_gcs RENAME COLUMN geom TO geometry;")
  # x = "eez_gcs"
  # y = "testNW"
  # z = "eez_nw"

  # x = "boem_lease_blocks"
  # y = "testNW"
  # z = "boem_lease_blocks_nw"

  # x="blocks_nw"; y="eez_gcs"; z="blocks_eez_nw"

  i = glue("{z}_gix")

  x_flds <- dbListFields(con, x) %>% setdiff("geometry")
  y_flds <- dbListFields(con, y) %>% setdiff("geometry")
  flds <- c(x_flds, y_flds)
  if (any(duplicated(flds))){
    stop("TODO: deal with duplicates")
  }
  sql <- glue_sql("
                  DROP TABLE IF EXISTS {`z`};
                  CREATE TABLE {`z`} AS
                  SELECT {`flds`*},
                  CASE
                  WHEN ST_CoveredBy(a.geometry, b.geometry) THEN
                  ST_Multi(ST_CollectionExtract(a.geometry, 3))
                  ELSE
                  ST_Multi(ST_CollectionExtract(ST_Intersection(a.geometry, b.geometry), 3))
                  END AS geometry
                  FROM {`x`} AS a
                  INNER JOIN {`y`} AS b
                  ON (ST_Intersects(a.geometry, b.geometry)
                  AND NOT ST_Touches(a.geometry, b.geometry) );
                  ALTER TABLE eez ALTER COLUMN geometry TYPE Geometry(MultiPolygon, 4326) USING ST_Multi(geometry);
                  CREATE INDEX {`i`} ON {`z`} USING GIST (geometry);", .con = con)

  # sql <- glue_sql("
  #   DROP TABLE IF EXISTS {`z`};
  #   CREATE TABLE {`z`} AS
  #   SELECT {`flds`*}, ST_Multi(ST_Intersection(
  #     ST_CollectionExtract(a.geometry, 3),
  #     ST_CollectionExtract(b.geometry, 3))) AS geometry
  #   FROM {`x`} AS A, {`y`} AS b
  #   WHERE ST_Intersects(a.geometry, b.geometry);
  #   CREATE INDEX {`i`} ON {`z`} USING GIST (geometry);", .con = con)

  # ST_Multi
  # x <- "boem_lease_blocks_nw"
  # x <- "g_hyxcbe"
  # # SELECT blk_id, ST_GeometryType(ST_Multi(ST_CollectionExtract(geometry, 3)) AS geom_type, geometry
  # blk_geomtype_sql <- glue_sql("
  #   SELECT blk_id, ST_GeometryType(geometry) AS geom_type, geometry
  #   FROM {`x`};", .con = con)
  # cat(blk_geomtype_sql)
  # blk_geomtype <- st_read(con, query = blk_geomtype_sql)
  # table(blk_geomtype$geom_type)
  # ST_GeometryCollection       ST_MultiPolygon            ST_Polygon
  #                  3333                  3418                  2346

  # eez_geomtype_sql <- glue_sql("
  #   SELECT territory, ST_GeometryType(geometry) AS geom_type, geometry
  #   FROM {`y`};", .con = con)
  # cat(eez_geomtype_sql)
  # eez <- st_read(con, query = eez_geomtype_sql)
  # ST_MultiPolygon
  #eez <- st_read(con, "eez_gcs")

  #cat(sql)
  r <- dbSendQuery(con, sql)
}

pg_erase <- function(con, x, y, z){

  i = glue("{z}_gix")

  flds <- dbListFields(con, x) %>% setdiff("geometry")
  #a.flds <- paste(sprintf("a.%s", flds), collapse=', ')
  sql <- glue_sql("
                  DROP TABLE IF EXISTS {`z`};
                  CREATE TABLE {`z`} AS
                  SELECT {`flds`*},
                  ST_Multi(ST_CollectionExtract(ST_Difference(a.geometry, b.geometry), 3)) as geometry
                  FROM
                  (SELECT ST_Union(geometry) as geometry FROM {`y`}) AS b,
                  {`x`} as a
                  WHERE a.geometry && b.geometry;
                  CREATE INDEX {`i`} ON {`z`} USING GIST (geometry);", .con = con)
  # sql <- glue_sql("
  #   DROP TABLE IF EXISTS {`z`};
  #   CREATE TABLE {`z`} AS
  #   SELECT {`flds`*},
  #     CASE
  #       WHEN ST_Intersects(a.geometry, b.geometry) THEN
  #         ST_Difference(a.geometry, b.geometry)
  #       ELSE
  #         a.geometry
  #     END as geometry
  #   FROM
  #     {`x`} as a,
  #     (SELECT ST_Union(geometry) as geometry FROM {`y`}) AS b
  #   WHERE (a.geometry && b.geometry) AND NOT (ST_Within(a.geometry, b.geometry));
  #   CREATE INDEX {`i`} ON {`z`} USING GIST (geometry);")
  cat(sql)
  r <- dbSendQuery(con, sql)

  z_sf <- st_read(con, z)
  #mapview::mapview(z_sf)


}

idx_tbl <- function(tbl, idx_flds = NULL, geom_ply=T){
  # tbl = "g_hyxcb"

  get_fld_type <- function(fld){
    suppressWarnings({
      r <- dbGetQuery(con, glue_sql('SELECT pg_typeof({`fld`}) from {`tbl`} limit 1;', .con=con)) %>%
        pull(pg_typeof)
    })
    r
  }

  flds <- tibble(
    fld = dbListFields(con, tbl)) %>%
    mutate(
      type = map_chr(fld, get_fld_type))

  fld_geom <- flds %>% filter(type=="geometry") %>% pull(fld)
  if (length(fld_geom) == 1 & fld_geom != "geometry"){
    cat(glue("RENAME {tbl}.{fld_geom} TO geometry\n"))
    dbSendQuery(con, glue_sql("ALTER TABLE {`tbl`} RENAME COLUMN {`fld_geom`} TO geometry;", .con = con))
  }

  idx_geom <- glue("{tbl}_gix")
  idxs <- dbGetQuery(con, glue_sql("SELECT * FROM pg_indexes;", .con=con)) %>%
    as_tibble() %>%
    filter(tablename == tbl)
  if (length(fld_geom) == 1 & !idx_geom %in% idxs$indexname){
    cat(glue("CREATE INDEX ON {tbl} (geometry) USING GIST\n"))
    dbSendQuery(con, glue("CREATE INDEX {idx_geom} ON {tbl} USING GIST (geometry);"))
  }

  if (length(fld_geom) == 1 & geom_ply){
    cat(glue("ALTER {tbl}.geometry TYPE Geometry(MultiPolygon, 4326) USING ST_Multi(geometry)\n"))
    dbSendQuery(con, glue("ALTER TABLE {tbl} ALTER COLUMN geometry TYPE Geometry(MultiPolygon, 4326) USING ST_Multi(geometry);"))
  }

  if (!is.null(idx_flds)){
    for (fld in idx_flds){
      idx <- glue("{tbl}_{fld}_ix")
      if (!idx %in% idxs$indexname){
        cat(glue("CREATE INDEX ON {tbl} ({fld})\n"))
        dbSendQuery(con, glue("CREATE INDEX {idx} ON {tbl} ({fld});"))
      }
    }
  }
}

# prep ----
if (!file.exists(eez_u_shp)){
  eez_gcs_sf   <- sf_read(eez_gcs_geo)

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
}

# test aws-nreldb
dbListTables(con)
eez_u <- read_sf(eez_u_shp)
#dbSendQuery(con, "create extension postgis;")
dbWriteTable(con, "eez", eez_u)
idx_tbl("eez")

if (!file.exists(blk_shp)){
  blk <- rbind(
    sf_read(atl_shp), # 4269
    sf_read(gom_shp) %>%
      select(PROT_NUMBE, PROT_APRV_, BLOCK_NUMB, BLK_FED_AP, MMS_REGION, MMS_PLAN_A), # 4267
    sf_read(ak_shp) %>%
      select(PROT_NUMBE, PROT_APRV_, BLOCK_NUMB, BLK_FED_AP, MMS_REGION, MMS_PLAN_A),  # 4269
    sf_read(pac_shp) %>%
      select(PROT_NUMBE, PROT_APRV_, BLOCK_NUMB, BLK_FED_AP, MMS_REGION, MMS_PLAN_A)) # 4269
  write_sf(blk, blk_shp)
}

if (!file.exists(blk_eez_shp)){
  if (!exists("blk")) blk <- read_sf(blk_shp)

  # TODO: clip blocks outside eez
  blk_eez <- st_intersection(blk, eez_u)
  write_sf(blk, blk_eez_shp)
}

if (!file.exists(blk_todo_shp)){
  if (!exists("blk")) blk <- read_sf(blk_shp)

  blk_todo <- sf_erase(eez_u, blk)
  write_sf(blk_todo, blk_todo_shp)
}
#blk <- read_sf(blk_shp)

# g_hyx: hemisphere, latitudinal bands, UTM longitude ----
if (!file.exists(g_hyx_shp)){

  # UTM zones ----
  utm_zones_sf <- sf_read(utm_zones_shp)

  # g_h: (h)emisphere, N or South ----
  g_h <- st_make_grid(
    cellsize = c(360,90),
    offset = c(-180,-90), n = c(1,2), crs = st_crs(4326))
  #g_h = data.frame(i = 1:length(g_h)) %>%
  g_h = data.frame(h = c("S","N")) %>%
    mutate(
      geom = g_h) %>%
    st_as_sf()
  #plot(g_h)

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

  # g_hyx: intersection ----
  g_hyx <- st_intersection(g_h, g_y) %>%
    st_intersection(g_x)

  g_hyx <- g_hyx %>%
    mutate(
      geom_type = st_geometry_type(geom) %>% as.character()) %>%
    filter(geom_type == "POLYGON") %>%
    select(-geom_type)

  g_hyx <- g_hyx %>%
    sf_intersects(eez_u)
  write_sf(g_hyx, g_hyx_shp)
  #plot(g_hyxe["y"])
}

# g_*c map areas: Alaska 3deg + not Alaska 2deg ----
if (!file.exists(g_hyxc_shp)){

  g_hyx <- sf_read(g_hyx_shp)

  # tmp test Alaska ----
  # blk_ak_shp        <- here("data/layers/blk/usa_blk_ak.shp")
  # eez_ak <- eez_u %>% filter(territory == "Alaska")
  # plot(eez_ak)
  # # blk_ak <- read_sf(blk_shp) %>%
  # #   sf_intersects(eez_ak)
  #
  # # 5 min
  # dbSendQuery(con, "CREATE TABLE boem_lease_blocks_ak AS SELECT a.* FROM boem_lease_blocks AS a, (SELECT ST_Union(geometry) AS geometry FROM eez WHERE territory = 'Alaska') AS b WHERE ST_Intersects(a.geometry, b.geometry);")
  # fix_geom("boem_lease_blocks_ak")
  #
  # blk_ak <- st_read(con, "boem_lease_blocks_ak") %>%
  #   separate(
  #     blk_id, c("s", "p", "b"), sep = "_",
  #     remove = F, convert = T) %>%
  #   mutate(
  #     h = str_sub(p, 1, 1),
  #     y = str_sub(p, 2, 2),
  #     x = str_sub(p, 3, 4) %>% as.integer(),
  #     m = str_sub(p, 6, 7) %>% as.integer())
  # # blk_ak %>% st_set_geometry(NULL) %>% as_tibble()
  # write_sf(blk_ak, blk_ak_shp)


  # g_*a: map 1 of 8 for Alaska  ----
  g_hyxa <- g_hyx %>%
    sf_intersects(eez_u %>% filter(territory == "Alaska")) %>%
    mutate(
      row = row_number(),
      a = NA)

  m <- matrix(1:(2*4), nrow=4, ncol=2, byrow=T)
  v <- apply(m, 1, rev) %>% rev() %>% as.vector()
  for (i in 1:max(g_hyxa$row)){ # i <- 1
    g_i <- g_hyxa %>%
      filter(row == i)

    bb <- st_bbox(g_i) # plot(bb)
    g_im <- st_make_grid(
      cellsize = c(3,1),
      offset = c(bb$xmin, bb$ymin), n = c(2,4), crs = st_crs(4326))
    d_i <- st_set_geometry(g_i, NULL)
    g_im <- d_i[rep(seq_len(nrow(d_i)), each=2*4),] %>%
      mutate(
        geom = g_im,
        a    = v) %>%
      st_as_sf()
    # plot(g_im['a']) #, add=T)

    cat(glue("{i}: {nrow(g_hyxa)} - 1 + {nrow(g_im)} = ..."), "\n")
    g_hyxa <- g_hyxa %>%
      filter(row != i) %>%
      rbind(
        g_im)
    cat(glue("{i}: ... = {nrow(g_hyxa)}"), "\n")
  }
  write_sf(g_hyxa, g_hyxa_shp)


  # plot(g_hyx['y'])
  # plot(g_hyxa['m'])

  g_hyxa <- g_hyxa %>%
    sf_intersects(eez_u)
  write_sf(g_hyxa, g_hyxa_shp)
  g_hyxa <- sf_read(g_hyxa_shp)


  # g_*m: map 1 of 12, except Alaska ----
  g_hyxm <- g_hyx %>%
    sf_intersects(eez_u %>% filter(territory != "Alaska")) %>%
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
    sf_intersects(eez_u %>% filter(territory != "Alaska"))
  write_sf(g_hyxm, g_hyxm_shp)

  #g_hyxa <- sf_read(g_hyxa_shp)
  #g_hyxm <- sf_read(g_hyxm_shp)

  # g_hyxc: g_hyxa (Alaska; 3deg) + g_hyxm (not Alaska; 2deg) ----
  g_hyxc <- g_hyxa %>%
    mutate(m = NA) %>%
    rbind(
      g_hyxm %>%
        mutate(a = NA))
  write_sf(g_hyxc, g_hyxc_shp)
}

# tmp Alaska check ----
# dbSendQuery(con, "ALTER TABLE g_hyxc RENAME COLUMN geom TO geometry;")
# idx_tbl("g_hyxc")
# idx_tbl("eez_gcs")
# pg_intersection(con, "g_hyxc", "eez_gcs", "g_hyxcz")
#
# dbSendQuery(con, "
#   ALTER TABLE g_hyxc ADD COLUMN is_alaska BOOLEAN;
#   UPDATE g_hyxc SET is_alaska = ST_Intersects(g_hyxc.geometry, (SELECT geometry FROM eez_gcs WHERE territory = 'Alaska'));")

# g_*b block numbers in db ----
if (!"g_hyxcb" %in% dbListTables(con)){

  g_hyxc <- read_sf(g_hyxc_shp)
  #g_hyxc2 <- st_read(g_hyxc_shp) %>% as_tibble() %>% st_sf()

  # get row number to update
  g_hyxcb <- g_hyxc %>%
    select(-geom_type) %>%
    mutate(
      row = row_number())

  # g_hyxm <- g_hyxcb %>%
  #   filter(!is.na(m))

  dbWriteTable(con, "g_hyxcb", g_hyxcb, overwrite=T)
  idx_tbl("g_hyxcb", idx_flds = "row")
  dbSendQuery(con, "ALTER TABLE g_hyxcb ADD COLUMN b SMALLINT;")
  g_hyxcb <- st_read(con, "g_hyxcb")
  #g_hyxcb <- st_read(con, query="SELECT * FROM g_hyxcb LIMIT 3;") # , geometry_column="geom")
  #g_hyxcb %>% select(-m) %>% plot()
  #mapview::mapview(g_hyxcb) # %>% filter(row == 789) + eez_u

  # blocks: 3 nmi^2; 1 nmi = 1 min lat = 1/60 dd
  m <- t(sapply(seq(0, 50*19, by=50), function(x) x + 1:40 + 6000))
  v <- apply(m, 1, rev) %>% rev() %>% as.vector() %>% as.integer()

  t0 <- Sys.time()
  nr <- max(g_hyxcb$row)
  for (i in 1:nr){ # i <- 1 # i <- 425
    ti <- Sys.time()

    # data
    sf_i <- g_hyxcb %>% filter(row == i)
    df_i <- sf_i %>% st_set_geometry(NULL)

    # bounding box
    #st_read(dsn=con, query=glue_sql("SELECT h,y,x,row,a,m,b,geometry FROM g_hyxcb WHERE row = {i};")) # , geom_column="geometry")
    # bb <- dbGetQuery(con, glue_sql("SELECT ST_Extent(geometry)::TEXT AS bbox FROM g_hyxcb WHERE row = {i};")) %>%
    #   mutate(
    #     bbox = str_replace_all(bbox, "[^[0-9] ,-]", "")) %>%
    #   separate(
    #     bbox, sep = "[, ]", convert = T,
    #     into = c("xmin", "ymin", "xmax", "ymax"))
    bb <- st_bbox(sf_i)

    # x_width: Alaska 3deg span over x (longitude), vs not: 2deg
    x_width <- ifelse(bb$xmax - bb$xmin == 3, (9/2)/60, 3/60)
    geom <- st_make_grid(
        cellsize = c(x_width, 3/60),
        offset = c(bb$xmin, bb$ymin), n = c(2*20, 20), crs = st_crs(4326)) %>%
      st_cast("MULTIPOLYGON")
    g_ib <- df_i[rep(1, each=2*20*20),] %>%
      mutate(
        geometry = geom,
        b        = v) %>%
      as_tibble() %>% st_sf()

    #plot(g_ib['b'], add=T)
    # bb_sf <- st_sf(
    #   name = "bounding box for protraction diagrams of lease blocks",
    #   geometry =
    #     st_sfc(st_polygon(
    #       with(bb, list(
    #         matrix(c(
    #           xmin, ymin,
    #           xmax, ymin,
    #           xmax, ymax,
    #           xmin, ymax,
    #           xmin, ymin), ncol=2, byrow=T)))), crs = 4326)) # %>% st_sf()
    #
    # lims <- st_bbox(st_union(bb_sf, g_ib))
    # trans_red <- rgb(255, 0, 0, max = 255, alpha = round(255*30/100))
    # pal <- colorNumeric("Spectral", g_ib$b)
    # leaflet(g_ib) %>%
    #   addProviderTiles(providers$Stamen.TonerLite) %>%
    #   addPolygons(color = ~pal(b), weight=1) %>%
    #   addPolygons(data=bb_sf, color="red", fill=F)
    # st_bbox(bb_sf)
    # st_bbox(g_ib)

    # update db
    dbSendQuery(con, glue_sql("DELETE FROM g_hyxcb WHERE row = {i};"))
    dbWriteTable(con, "g_hyxcb", g_ib, append=T) # ?st_write

    # report time
    tn <- Sys.time()
    eta <- tn + ((nr-i) * (tn - t0) / (i))
    cat(sprintf("%04d of %d: %0.4f secs, eta %s\n", i, nr, difftime(tn, ti, units = "secs"), eta))
  }

  # TODO: log protraction number, block number
  idx_tbl("g_hyxcb")
  # PROT_NUMBE, PROT_APRV_, BLOCK_NUMB, BLK_FED_AP, MMS_REGION, MMS_PLAN_A
  #r <- dbSendQuery(con, "ALTER TABLE g_hyxcb ADD COLUMN PROT_NUMBE TEXT, BLOCK_NUMB INTEGER ;")
  #r <- dbSendQuery(con, "CREATE INDEX ON g_hyxcb (row);")
}

# TODO: parallelize intersection queries by territory

# test NW box: create blocks ----
# b <- sf_box(-131, 46, -124, 49, "testNW")
# mapview::mapview(b)
# dbWriteTable(con, "testNW", b, overwrite=T)
# pg_intersection(con, "g_hyxcb", "testNW", "g_hyxcb_nw")
# #dbSendQuery(con, "ALTER TABLE eez_gcs RENAME COLUMN geom TO geometry;")
# pg_intersection(con, "eez_gcs", "testNW", "eez_nw")
# pg_intersection(con, "boem_lease_blocks", "testNW", "boem_lease_blocks_nw")

# blocks: BOEM leases + NREL created ----
if (!"blocks" %in% dbListTables(con)){

  boem_lease_blocks <- read_sf(blk_shp) %>%
    mutate(blk_id = glue("B_{PROT_NUMBE}_{BLOCK_NUMB}")) %>%
    separate(
      blk_id, c("s", "p", "b"), sep = "_", remove = F, convert = T) %>%
    mutate(
      h = str_sub(p, 1, 1),
      y = str_sub(p, 2, 2),
      x = str_sub(p, 3, 4) %>% as.integer(),
      m = str_sub(p, 6, 7) %>% as.integer()) %>%
    select(blk_id, s, p, b, h, y, x, m)
  dbWriteTable(con, "boem_lease_blocks", boem_lease_blocks, overwrite=T)
  idx_tbl("boem_lease_blocks", "blk_id")

  # blk <- read_sf(blk_shp)
  # table(blk$MMS_REGION)
  #      A      G      P      Y
  #  48446  29187  44627 187581
  # A: Atlantic
  # G: Gulf of Mexico
  # P: Pacific
  # Y: Alaska
  # blk %>% filter(MMS_REGION=="Y") %>% mapview::mapview()

  # table(blk$MMS_PLAN_A)
  #   ALA   ALB   BFT   BOW   CEC   CGM   CHU   COK   EGM   FLS   GEO   GOA   HOP   KOD   MAT
  # 47061  7444 11876 15633  7808 12409 11472  1093 11538  1892 12625 20486  2456 15949  9737
  #   MDA   NAL   NAV   NOA   NOC   NOR   SHU   SOA   SOC   WAO   WGM
  # 20098  5956  6127 16660  8002  4608 15058  9796 16032 12785  5240

  g_hyxcbs <- st_read(con, "g_hyxcb") %>%
    mutate(
      c = if_else(is.na(m), a, m),
      blk_id = sprintf("N_%s%s%02d-%02d_%d", h, y, x, c, b)) %>%
    separate(
      blk_id, c("s", "p", "b"), sep = "_", remove = F, convert = T) %>%
    mutate(
      h = str_sub(p, 1, 1),
      y = str_sub(p, 2, 2),
      x = str_sub(p, 3, 4) %>% as.integer(),
      m = str_sub(p, 6, 7) %>% as.integer()) %>%
    select(blk_id, s, p, b, h, y, x, m)
  dbWriteTable(con, "g_hyxcbs", g_hyxcbs, overwrite=T)
  idx_tbl("g_hyxcbs", "blk_id")

  # intersect with eez
  system.time({
    pg_intersection(con, "blocks", "eez", "blocks_z")
    idx_tbl("blocks", c("blk_id", "territory"))
  })
  system.time({
    pg_intersection(con, "g_hyxcbs", "eez", "g_hyxcbsz")
    idx_tbl("g_hyxcbs", c("blk_id", "territory"))
  })



  system.time({
    eezs <- dbGetQuery(con, "SELECT eez FROM eez;") %>% pull("eez")
    territory <- "Gulf of Mexico"
    tbl_new <- "blocks_z"
    dbSendQuery(con, "CREATE TABLE {tbl_new} AS SELECT AS ")
    pg_erase(con, "g_hyxcb", "boem_lease_blocks", "g_hyxcbe")
  })

  # union
  system.time({
    dbSendQuery(con,
                "DROP TABLE IF EXISTS blocks;
                CREATE TABLE blocks AS
                SELECT blk_id, geometry FROM boem_lease_blocks
                UNION
                SELECT blk_id, geometry FROM g_hyxcbe;")
    idx_tbl("blocks")
    idx_tbl("blocks", "blk_id")
  })

  dbRemoveTable(con, "blocks")
  dbSendQuery(con, "DROP TABLE blocks; ALTER TABLE blocks_eez RENAME TO blocks;")
  idx_tbl("blocks"); idx_tbl("blocks", "blk_id")

  # TODO: intersect with NMS
}


q <- glue_sql(
  read_lines(here("scripts/x.sql")) %>% paste(collapse="\n"),
  A = "g_hyxcb", A_ID = "",
  B = "eez_gcs", B_ID = "territory",
  O = "x_eez_utm", .con = con)
r <- dbGetQuery(con, q)
r


# TODO: index g_hyxcb gist in pg db, intersect with eez
# TODO: lease block names

# TODO: update Alaska with g_*x every 3 (vs 2) degrees

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



