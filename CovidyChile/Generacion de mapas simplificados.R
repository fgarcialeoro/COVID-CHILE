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
library(rgeos)


divisiones_administrativas<- readOGR("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/DivisionPoliticoAdministrativa2019/DivisionPoliticaAdministrativa2019.shp", GDAL1_integer64_policy = TRUE)
divisiones <- spTransform(divisiones_administrativas, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))

regiones<-as.character(unique(divisiones$CUT_REG))

mapa<-subset(divisiones, divisiones$CUT_REG==regiones[1])
mapa<-ms_simplify(mapa, keep = 0.001,sys = TRUE)

for (i in c(2:10,13:16)){
  print(i)
  region_i<-subset(divisiones, divisiones$CUT_REG==regiones[i])
  region_i<-ms_simplify(region_i, keep = 0.001)
  mapa<-rbind(region_i,mapa)}

region_11<-subset(divisiones, divisiones$CUT_REG==regiones[11],sys = TRUE)
region_11<-ms_simplify(region_11, keep = 0.001,sys = TRUE)

region_12<-subset(divisiones, divisiones$CUT_REG==regiones[12])
region_12<-ms_simplify(region_12, keep = 0.00001,sys = TRUE,snap=FALSE,weighting=0,no_repair=TRUE)


mapa<-rbind(region_11,mapa)
mapa<-rbind(region_12,mapa)

mapa@data$CUT_COM<-as.numeric(as.character(mapa@data$CUT_COM))
mapa@data$CUT_REG<-as.numeric(mapa@data$CUT_REG)

saveRDS(mapa,"www/mapa.shp")








