

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
options(scipen = 999)
rm(list = ls())

# Load data ---------------------------------------------------------------
col <- st_read('../shapefile/base/MGN_DPTO_POLITICO.shp')
occ_wrl <- read_csv('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_points/_global/Presence_data_all_2019_oct.csv')

# Selecting the departments target ----------------------------------------
occ_col <- occ_wrl %>% filter(Country == 'Colombia')
dpt <- filter(col, DPTO_CNMBR %in% c('CAQUETÁ', 'CAUCA', 'META', 'RISARALDA', 'ANTIOQUIA', 'MAGDALENA', 'HUILA'))
occ_col <- st_as_sf(occ_col, coords = c('Longitude', 'Latitude'))
st_write(obj = occ_col, dsn = '../shapefile/presences', layer = 'presences_colombia', driver = 'ESRI Shapefile')

occ_cff <- read_csv('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_coffeeColombia/tbl/points/run1')

# Filtering presences for the departments ---------------------------------
st_crs(occ_col) <- st_crs(dpt)
occ_dpt <- st_intersection(x = occ_col, y = dpt)
unique(dpt$DPTO_CNMBR) %>% as.character()
unique(occ_dpt$DPTO_CNMBR) %>% as.character()

table(occ_dpt$DPTO_CNMBR) %>% as.data.frame %>% filter(Freq > 0)
