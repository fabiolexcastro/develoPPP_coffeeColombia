

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, readxl, readxlsb)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
prd <- read_excel('../tablas/production/EVA.xlsx') 
adm <- st_read('../shapefile/base/MGN_MPIO_POLITICO.shp')
adm <- adm %>% mutate(code_mpio = paste0(DPTO_CCDGO, MPIO_CCDGO) %>% as.numeric())

# Filtering only coffee ---------------------------------------------------
sort(unique(prd$CULTIVO))
yrs <- unique(prd$AÑO)
prd <- prd %>% 
  filter(CULTIVO == 'CAFE' &
           PERIODO %in% c(2015:2019))
prd <- prd[,c(2, 3, 4, 6, 9, 11, 12, 13, 14)]
names(prd) <- c('dpto', 'code_mpio', 'mpio', 'subgrupo', 'anio', 'sembrada', 'cosechada', 'produccion', 'rdto')

write.csv(prd, '../tablas/production/EVA_2015_2019.csv', row.names = FALSE)

# N mpios by each year ----------------------------------------------------
gg_count <- prd %>% 
  group_by(anio) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  ggplot(data = ., aes(x = anio, y = count)) +
  geom_line() +
  labs(y = 'Cantidad municipios', x = '') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8))
ggsave(plot = gg_count, 
       filename = '../png/graphics/Conteo cultivo mpios by year.png', units = 'cm', width = 12, height = 10, dpi = 300)

# Calculating the average -------------------------------------------------
avg <- prd %>% 
  group_by(dpto, code_mpio, mpio) %>% 
  summarise(sembrada = mean(sembrada, na.rm = TRUE),
            cosechada = mean(cosechada, na.rm = TRUE),
            produccion = mean(produccion, na.rm = TRUE),
            rdto = mean(rdto, na.rm = TRUE)) %>% 
  ungroup()

adm <- inner_join(adm, avg, by = 'code_mpio')

shapefile(as(adm, 'Spatial'), '../shapefile/produccion/cafe_produccion_2015_2019.shp')
file.remove('../shapefile/produccion/cacao_produccion_2015_2019.shx')

