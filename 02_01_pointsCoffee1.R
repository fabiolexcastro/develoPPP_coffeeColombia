

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

occ_cff <- read_csv('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_coffeeColombia/tbl/points/run1/points_all.csv')
occ_cff <- st_as_sf(occ_cff, coords = c('Longitude', 'Latitude'))
st_crs(occ_cff) <- st_crs(dpt)
st_crs(occ_col) <- st_crs(dpt)
occ_cff <- st_intersection(x = occ_cff, y = col)
occ_col <- st_intersection(x = occ_col, y = col)

# Join the both datasets
occ_cff <- occ_cff %>% mutate(longitude = st_coordinates(occ_cff)[,1], latitude = st_coordinates(occ_cff)[,2])
occ_col <- occ_col %>% mutate(longitude = st_coordinates(occ_col)[,1], latitude = st_coordinates(occ_col)[,2])
occ_cff <- occ_cff %>% as.data.frame %>% dplyr::select(DPTO_CNMBR, longitude, latitude) %>% as_tibble()
occ_col <- occ_col %>% as.data.frame %>% dplyr::select(DPTO_CNMBR, longitude, latitude) %>% as_tibble()
occ <- rbind(occ_cff, occ_col)
occ <- occ[!duplicated(occ),]

occ %>% 
  group_by(DPTO_CNMBR) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))

occ <- occ %>% filter(DPTO_CNMBR %in% c('CAQUETÁ', 'CAUCA', 'META', 'RISARALDA', 'ANTIOQUIA', 'MAGDALENA', 'HUILA'))

# Loading the mask --------------------------------------------------------
msk <- raster('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cocoaSouth/tif/climate/WORLDCLIM/crn/cnts/bio_1.asc') * 0
msk <- raster::crop(msk, as(dpt, 'Spatial')) %>% raster::mask(., as(dpt, 'Spatial'))

# Removing duplicated by cell ---------------------------------------------
cellNum <- raster::extract(msk, occ[,c('longitude', 'latitude')], cellnumbers = T) 
cells <- xyFromCell(msk, cellNum[,'cells'])
dupvec <- duplicated(cells[,c('x', 'y')])
occ_rmDupCell <- tbl_df(occ[!dupvec,])
occ_DupCell <- tbl_df(occ[dupvec,])

write.csv(occ_rmDupCell, '../tablas/points/points_rmDup.csv', row.names = FALSE)

# Table to shapefile
coordinates(occ_rmDupCell) <- ~ longitude + latitude
shapefile(occ_rmDupCell, '../shapefile/presences/presences_colombia_rmDupCell.shp')

occ_rmDupCell %>% 
  as.data.frame %>% 
  group_by(DPTO_CNMBR) %>% 
  summarise(count = n()) %>% 
  ungroup()


