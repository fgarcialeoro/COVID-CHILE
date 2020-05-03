library(xlsxjars)
library(xlsx)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(lubridate)
library(googleVis)
library(chilemapas)
library(scales)
library(plotly)
library(rgdal)
library(leaflet)
library(sp)
library(rmapshaper)
library(stringi)


divisiones_administrativas<- readOGR("/Volumes/Respaldo/carpeta\ para\ dropbox/Dropbox/working\ directory\ R/COVID\ CHILE/datos/division_comunal_geo_ide_1/division_comunal_geo_ide_1.shp", GDAL1_integer64_policy = TRUE)
divisiones <- spTransform(divisiones_administrativas, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

regiones<-as.character(unique(divisiones$NOM_REG))

mapa<-subset(divisiones,divisiones$NOM_REG==regiones[1])
mapa<-ms_simplify(mapa, keep = 0.001)

for (i in c(2:17)){
  print(i)
  region_i<-subset(divisiones, divisiones$NOM_REG==regiones[i])
  region_i<-ms_simplify(region_i, keep = 0.001)
  mapa<-rbind(region_i,mapa)}


mapa@data$cod_comuna<-as.numeric(as.character(mapa@data$cod_comuna))
mapa@data$codregion<-as.numeric(mapa@data$codregion)

saveRDS(mapa,"www/mapa_bcn.shp")


