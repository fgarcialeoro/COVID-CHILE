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

mapa_actuales<-function(fecha,region){
  
casos_actuales_comuna<-casos_actuales_comuna()
casos_actuales_fecha_region<-filter(casos_actuales_comuna, Fecha==fecha & Region==Region)
codigo_de_region<-unique(filter(select(casos_actuales_fecha_region,Region,`Codigo region`),Region==region))[1,2]
mapa<- readRDS("www/mapa.shp")
mapa<-subset(mapa, mapa$CUT_REG==codigo_de_region)
mapa@data<-left_join(mapa@data,casos_actuales_fecha_region,by=c("CUT_COM"="Codigo comuna"))

bins <- c(0,1,10,20,40,80,160,320,640,1200, Inf)
pal <- colorBin("YlOrRd", domain = mapa$`Casos actuales cada 100000 habitantes`, bins = bins)   
leaflet(mapa)%>%
  addPolygons(fillColor = ~pal(`Casos actuales cada 100000 habitantes`),weight =5,opacity = 1,color = "black",dashArray = "3",fillOpacity = 0.7,label=~COMUNA)%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLegend(pal = pal, values = ~`Casos actuales cada 100000 habitantes`, opacity = 0.7, title = NULL,position = "bottomright")
}










































