

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, foreach, doSNOW, parallel, stars, hablar, agricolae)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------




# Load data ---------------------------------------------------------------

# Presences
occ <- read_csv('../tablas/points/points_rmDup.csv')
occ <- occ %>% mutate(DPTO_CNMBR = iconv(DPTO_CNMBR, to = 'latin1'))

# Climate
crn <- list.files('../raster/climate/current/wc/dptos', full.names = T, pattern = '.tif$')
crn <- mixedsort(crn)
vrs <- paste0('bio_', 1:20, '.tif')
crn <- grep(paste0(vrs, collapse = '|'), crn, value = T)
crn <- stack(crn)

# Extract the climate values for the presences ----------------------------
vls <- raster::extract(crn, occ[,2:3])
vls <- cbind(occ[,2:3], vls)
vls <- as_tibble(vls)


# Outlier analysis --------------------------------------------------------


