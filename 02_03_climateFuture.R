
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, foreach, doSNOW, parallel, stars, hablar, agricolae)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
extract_mask <- function(gc, yr){
  
  # gc <- gcm[1]
  # yr <- yrs[1]
  
  inp <- paste0(pth, '/', yr, '/', gc) %>% 
    list.files(., full.names = T, pattern = '.asc$') %>% 
    mixedsort()
  rst <- map(.x = inp, .f = raster)
  
  rst <- map(.x = 1:length(rst), .f = function(k){
    
    print(k)
    rst <- raster::crop(rst[[k]], dpt)
    rst <- raster::mask(rst, dpt)
    print('Done')
    return(rst)
    
  })
  
  nms <- names(stack(rst))
  Map('writeRaster', x = rst, filename = paste0('../raster/climate/future/wc/rcp60/', yr, '/', gc, '/', nms,  '.tif'), overwrite = TRUE)
  print('Done!')
  
}
calc_bio20 <- function(gc){
  
  ftr <- grep(paste0('/', gc, '/'), fls, value = T)
  pre <- stack(ftr)
  bin <- reclassify(pre, c(-Inf, 40, 1, 40, Inf, NA))
  names(bin) <- names(pre)
  twoyears   <- addLayer(bin, bin)
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
  bio_20 <- raster::mask(bio_20, pre[[1]])
  print('Write Raster')
  writeRaster(bio_20, paste0('../raster/climate/future/wc/rcp60/', '2040_2069/', gc, '/bio_20.tif'), overwrite = TRUE)
  
  
}

# Load data ---------------------------------------------------------------

pth <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cocoaSouth/tif/climate/WORLDCLIM/ftr/CMIP5'
yrs <- c('2020_2049', '2040_2069')

col <- st_read('../shapefile/base/MGN_DPTO_POLITICO.shp')
dpt <- filter(col, DPTO_CNMBR %in% c('CAQUETÁ', 'CAUCA', 'META', 'RISARALDA', 'ANTIOQUIA', 'MAGDALENA', 'HUILA'))
dpt <- as(dpt, 'Spatial')

gcm <- paste0(pth, '/', yrs[1]) %>% 
  list.files()

lapply(1:19, function(k) paste0('../raster/climate/future/wc/rcp60/2040_2069/', gcm[k]) %>% dir.create())

# Extract by mask ---------------------------------------------------------
cl <- makeCluster(18)
registerDoSNOW(cl)

foreach(i = 1:19, .verbose = T) %dopar% {
  
  require(pacman)
  pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, foreach, doSNOW, parallel)
  # extract_mask(gc = gcm[i], yr = '2020_2049')
  extract_mask(gc = gcm[i], yr = '2040_2069')
  
}

stopCluster(cl)

# Create function boclimatic 20 -------------------------------------------
fls <- list.files('../raster/climate/future/wc/rcp60/2040_2069/', full.names = TRUE)
fls <- lapply(fls, function(k) list.files(k, full.names = T, pattern = '.tif$'))
fls <- unlist(fls)
fls <- grep('prec', fls, value = T)
fls <- mixedsort(fls)

cl <- makeCluster(5)
registerDoSNOW(cl)

foreach(i = 1:19, .verbose = T) %dopar% {
  
  require(pacman)
  pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, foreach, doSNOW, parallel)
  calc_bio20(gc = gcm[i])
  
}
 
stopCluster(cl)
