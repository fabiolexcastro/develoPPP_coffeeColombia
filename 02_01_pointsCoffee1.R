

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
options(scipen = 999)
rm(list = ls())

# Load data ---------------------------------------------------------------
col <- st_read('../shapefile/base/MGN_DPTO_POLITICO.shp')
occ <- read_csv('../tablas/points/points_all_v1.csv')
dpt <- filter(col, DPTO_CNMBR %in% c('CAQUETÁ', 'CAUCA', 'META', 'RISARALDA', 'ANTIOQUIA', 'MAGDALENA', 'HUILA'))

# Loading the mask --------------------------------------------------------
msk <- raster('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cocoaSouth/tif/climate/WORLDCLIM/crn/cnts/bio_1.asc') * 0
msk <- raster::crop(msk, as(dpt, 'Spatial')) %>% raster::mask(., as(dpt, 'Spatial'))

# Removing duplicated by cell ---------------------------------------------
cellNum <- raster::extract(msk, occ[,c('x', 'y')], cellnumbers = T) 
cells <- xyFromCell(msk, cellNum[,'cells'])
dupvec <- duplicated(cells[,c('x', 'y')])
occ_rmDupCell <- tbl_df(occ[!dupvec,])
occ_DupCell <- tbl_df(occ[dupvec,])

write.csv(occ_rmDupCell, '../tablas/points/points_rmDup.csv', row.names = FALSE)

# Table to shapefile
coordinates(occ_rmDupCell) <- ~ x + y
shapefile(occ_rmDupCell, '../shapefile/presences/presences_colombia_rmDupCell.shp', overwrite = TRUE)

# Extracting the name of the departments ----------------------------------
dpt <- as(dpt, 'Spatial')
occ_dpt <- raster::intersect(occ_rmDupCell, dpt)

smm <- as.data.frame(occ_dpt) %>% 
  dplyr::select(id, x, y, dpto = DPTO_CNMBR) %>% 
  as_tibble() %>% 
  mutate(gid = 1:nrow(.)) %>% 
  group_by(dpto) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  mutate(dpto = str_to_title(dpto)) %>% 
  mutate(dpto = factor(dpto, levels = c('Huila', 'Cauca', 'Antioquia', 'Risaralda', 'Caquetá', 'Magdalena', 'Meta')))


gg <- ggplot(data = smm, aes(x = dpto, y = count)) +
  geom_col() +
  labs(x = '', y = 'Coffee presences (n)', caption = 'Adapted from FNC, 2020') +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, angle = 90, hjust = 0.5),
        axis.title.y = element_text(size = 12, face = 'bold')) +
  scale_y_continuous(labels = function(k) format(k, big.mark = ",", scientific = FALSE))

ggsave(plot = gg, filename = '../png/graphics/Coffee presences by Dpto.png' ,units = 'in', width = 9, height = 7, dpi = 300)
