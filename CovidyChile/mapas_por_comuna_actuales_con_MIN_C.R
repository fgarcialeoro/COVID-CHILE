library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(plotly)
library(rgdal)
library(leaflet)
library(sp)
library(rmapshaper)
library(stringi)


source("casos_actuales_comuna_con_MIN_C.R")

casos_actuales_comuna<-casos_actuales_comuna()

mapa<- readRDS("www/mapa.shp")


fechas<-unique(casos_actuales_comuna$Fecha)
regiones<-unique(casos_actuales_comuna$`Codigo region`)

for (i in (1:length(fechas))){
  casos_actuales_comuna_i<-subset(casos_actuales_comuna, casos_actuales_comuna$Fecha==fechas[i])
  
  for (j in (1:length(regiones))){
    casos_actuales_comuna_i_j<-subset(casos_actuales_comuna_i, casos_actuales_comuna_i$`Codigo region`==regiones[j])
    mapa_j<-subset(mapa, mapa$CUT_REG==regiones[j])
    mapa_i_j<-mapa_j
    mapa_i_j@data<-left_join(mapa_j@data,casos_actuales_comuna_i_j,by=c("CUT_COM"="Codigo comuna"))
    saveRDS(mapa_i_j,paste("www/mapas/mapa_chile",toupper(stri_trans_general(regiones[j], "Latin-ASCII")),fechas[i],sep="-"))
  }
}
rm(mapa,mapa_i_j,region_11,region_12,region_i,mapa_j,casos_actuales_comuna_i,casos_actuales_comuna_i_j,divisiones,divisiones_administrativas)













































