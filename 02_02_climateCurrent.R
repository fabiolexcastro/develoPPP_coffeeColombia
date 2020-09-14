

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load dataa --------------------------------------------------------------

pth <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cocoaSouth/tif/climate/WORLDCLIM/crn/cnts'
fls <- list.files(pth, full.names = T, pattern = '.asc$')
fls <- mixedsort(fls)

col <- st_read('../shapefile/base/MGN_DPTO_POLITICO.shp')
dpt <- filter(col, DPTO_CNMBR %in% c('CAQUETÁ', 'CAUCA', 'META', 'RISARALDA', 'ANTIOQUIA', 'MAGDALENA', 'HUILA'))
dpt <- as(dpt, 'Spatial')

rst <- map(.x = fls, .f = raster)

# Extract by mask ---------------------------------------------------------
rst <- map(.x = 1:length(rst), .f = function(k){
  
  print(k)
  rst <- raster::crop(rst[[k]], dpt)
  rst <- raster::mask(rst, dpt)
  print('Done')
  return(rst)
  
})

for(i in 1:length(rst)){
  print(i)
  writeRaster(x = rst[[i]], filename = paste0('../raster/climate/current/wc/dptos/', names(stack(rst))[[i]], '.tif'), overwrite = TRUE)
}

rm(rst)

# Calculating variable bioclimatic 20 -------------------------------------
prec <- list.files('../raster/climate/current/wc/dptos', full.names = T, pattern = '.tif$')
prec <- grep('prec', prec, value = TRUE)
prec <- mixedsort(prec)
prec <- stack(prec)

thrs <- 40

precbin <- reclassify(prec, c(-Inf, thrs, 1, thrs, Inf, NA))
names(precbin) <- names(prec)
twoyears   <- addLayer(precbin, precbin)
allperiods <- stack()
  
print('Stack ...')

for(i in 1:12){
    
    oneyear <- twoyears[[i:(i+11)]]
    drymonths <- cumsum(oneyear)
    maxnumber <- max(drymonths, na.rm = T)
    allperiods <- addLayer(allperiods, maxnumber)
    
    rm(maxnumber)
    rm(drymonths)
    
}

bio_20 <- max(allperiods, na.rm = T)
bio_20[is.na(bio_20)] <- 0
bio_20 <- raster::mask(bio_20, prec[[1]])
print('Write Raster')
writeRaster(bio_20, '../raster/climate/current/wc/dptos/bio_20.tif', overwrite = TRUE)
