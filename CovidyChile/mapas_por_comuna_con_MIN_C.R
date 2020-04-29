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

setwd("~/Dropbox/working directory R/COVID CHILE/CovidyChile")


confirmados_acumulados_comuna<-read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19.csv",check.names = FALSE,header = TRUE)
confirmados_acumulados_comuna<-gather(confirmados_acumulados_comuna,key="Fecha",value = "casos","2020-03-30":(ncol(confirmados_acumulados_comuna)-1))
confirmados_acumulados_comuna[, ]<-lapply(confirmados_acumulados_comuna[, ], as.character)
confirmados_acumulados_comuna$Poblacion<-as.numeric(confirmados_acumulados_comuna$Poblacion)
confirmados_acumulados_comuna$casos<-as.numeric(confirmados_acumulados_comuna$casos)
confirmados_acumulados_comuna$Fecha<-as.Date(confirmados_acumulados_comuna$Fecha)
confirmados_acumulados_comuna$`Codigo comuna`<-as.numeric(confirmados_acumulados_comuna$`Codigo comuna`)
confirmados_acumulados_comuna$`Codigo region`<-as.numeric(confirmados_acumulados_comuna$`Codigo region`)
confirmados_acumulados_comuna$Poblacion<-as.numeric(confirmados_acumulados_comuna$Poblacion)
colnames(confirmados_acumulados_comuna)[which(names(confirmados_acumulados_comuna) == "casos")] <- "Casos confirmados acumulados"
confirmados_acumulados_comuna$"Casos confirmados acumulados cada 100000 habitantes"<-(confirmados_acumulados_comuna$`Casos confirmados acumulados`/avance_contagiados_chile$Poblacion)*100000


################################################################################################################################
############################Casos activos por comuna
casos_actuales<-read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto25/CasosActualesPorComuna.csv",header=T,check.names=F)
casos_actuales<-filter(casos_actuales,Comuna!="Total")
casos_actuales<-gather(casos_actuales,key="Fecha",value = "Casos actuales","2020-04-13":(ncol(casos_actuales)))
casos_actuales[, ]<-lapply(casos_actuales[, ],as.character)
casos_actuales$Poblacion<-as.numeric(casos_actuales$Poblacion)
casos_actuales$`Casos actuales`<-as.numeric(casos_actuales$`Casos actuales`)
casos_actuales$Fecha<-as.Date(casos_actuales$Fecha)
casos_actuales$`Codigo comuna`<-as.numeric(casos_actuales$`Codigo comuna`)
casos_actuales$`Codigo region`<-as.numeric(casos_actuales$`Codigo region`)
casos_actuales$"Casos actuales cada 100000 habitantes"<-(casos_actuales$`Casos actuales`/casos_actuales$Poblacion)*100000


actuales_acumulados<-left_join(avance_contagiados_chile,casos_actuales)
write.csv(actuales_acumulados,"www/actuales_acumulados.csv",row.names = FALSE)






##https://data-flair.training/blogs/r-nonlinear-regression/
##Asym+(R0-Asym)*exp(-exp(lrc)*input)
##apropos("^SS")


################################################################################################
##############################Mapas por comuna##################################################


divisiones_administrativas<- readRDS("www/mapa.shp")


fechas<-unique(avance_contagiados_chile$Fecha)
regiones<-unique(avance_contagiados_chile$`Codigo region`)

for (i in (1:length(fechas))){
  avance_contagiados_chile_i<-subset(avance_contagiados_chile, avance_contagiados_chile$Fecha==fechas[i])
  
  for (j in (1:length(regiones))){
    avance_contagiados_chile_i_j<-subset(avance_contagiados_chile_i, avance_contagiados_chile_i$`Codigo region`==regiones[j])
    mapa_j<-subset(mapa, mapa$CUT_REG==regiones[j])
    mapa_i_j<-mapa_j
    mapa_i_j@data<-left_join(mapa_j@data,avance_contagiados_chile_i_j,by=c("CUT_COM"="Codigo comuna"))
    saveRDS(mapa_i_j,paste("www/mapas/mapa_chile",toupper(stri_trans_general(regiones[j], "Latin-ASCII")),fechas[i],sep="-"))
  }
}
rm(mapa,mapa_i_j,region_11,region_12,region_i,mapa_j,avance_contagiados_chile_i,avance_contagiados_chile_i_j,divisiones,divisiones_administrativas)




























































####################################################################################################################
####################################################################################################################
####################################################################################################################
#######################COMUNAS Y DATOS HABITACIONALES###############################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################


viviendas<-read.xlsx("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/censo/7_1_VIVIENDA.xls",sheetName ="Comuna")
viviendas<-mutate_all(viviendas,as.character())
nombre_de_columnas<-as.character(unlist(viviendas[1,]))
colnames(viviendas) <-nombre_de_columnas
viviendas<-viviendas[-1,]
viviendas<-filter(viviendas,`ÁREA`=="Total Comuna")
viviendas$ORDEN<-NULL
viviendas$`NOMBRE REGIÓN`<-NULL
viviendas$`CÓDIGO REGIÓN`<-NULL
viviendas$`NOMBRE PROVINCIA`<-NULL
viviendas$`CÓDIGO PROVINCIA`<-NULL
viviendas$`CÓDIGO COMUNA`<-NULL
viviendas$`TOTAL VIVIENDAS PARTICULARES`<-NULL
viviendas$ÁREA<-NULL
viviendas$`VIVIENDAS PARTICULARES DESOCUPADAS (en venta, para arriendo, abandonada u otro)`<-NULL
viviendas$`VIVIENDAS PARTICULARES DESOCUPADAS 
(de temporada)`<-NULL

viviendas<-gather(viviendas,`VIVIENDAS PARTICULARES OCUPADAS CON MORADORES PRESENTES`,`VIVIENDAS PARTICULARES OCUPADAS CON MORADORES AUSENTES`,key="Tipo de vivienda",value="Total")
viviendas$Total<-as.numeric(viviendas$Total)
viviendas<-aggregate(data=viviendas,Total~`NOMBRE COMUNA`,sum)







viviendas_2<-read.xlsx("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/censo/7_5_VIVIENDA.xls",sheetName ="Comuna")
viviendas_2<-mutate_all(viviendas_2,as.character())
nombre_de_columnas<-as.character(unlist(viviendas_2[1,]))
colnames(viviendas_2) <-nombre_de_columnas
viviendas_2<-viviendas_2[-1,]
viviendas_2$ORDEN<-NULL
viviendas_2$`NOMBRE REGIÓN`<-NULL
viviendas_2$`CÓDIGO REGIÓN`<-NULL
viviendas_2$`NOMBRE PROVINCIA`<-NULL
viviendas_2$`CÓDIGO PROVINCIA`<-NULL
viviendas_2$`CÓDIGO COMUNA`<-NULL
viviendas_2<-filter(viviendas_2,`NOMBRE COMUNA`!="PAÍS")
viviendas_2<-filter(viviendas_2,`CANTIDAD DE PERSONAS POR VIVIENDA`=="Total de Viviendas")

viviendas_2$`TOTAL VIVIENDAS PARTICULARES CON MORADORES PRESENTES`<-as.numeric(as.character(viviendas_2$`TOTAL VIVIENDAS PARTICULARES CON MORADORES PRESENTES`))
viviendas_2$`0 \nDORMITORIOS`<-as.numeric(as.character(viviendas_2$`0 \nDORMITORIOS`))
viviendas_2$`1 \nDORMITORIO`<-as.numeric(as.character(viviendas_2$`1 \nDORMITORIO`))
viviendas_2$`2 \nDORMITORIOS`<-as.numeric(as.character(viviendas_2$`2 \nDORMITORIOS`))
viviendas_2$`3 \nDORMITORIOS`<-as.numeric(as.character(viviendas_2$`3 \nDORMITORIOS`))
viviendas_2$`4 \nDORMITORIOS`<-as.numeric(as.character(viviendas_2$`4 \nDORMITORIOS`))
viviendas_2$`5 \nDORMITORIOS`<-as.numeric(as.character(viviendas_2$`5 \nDORMITORIOS`))
viviendas_2$`6 O MÁS DORMITORIOS `<-as.numeric(as.character(viviendas_2$`6 O MÁS DORMITORIOS `))
viviendas_2$`CANTIDAD DE DORMITORIOS IGNORADOS`<-as.numeric(as.character(viviendas_2$`CANTIDAD DE DORMITORIOS IGNORADOS`))
viviendas_2$Total<-viviendas_2$`TOTAL VIVIENDAS PARTICULARES CON MORADORES PRESENTES`-viviendas_2$`CANTIDAD DE DORMITORIOS IGNORADOS`

viviendas_2$Porcentaje_1<-(viviendas_2$`1 \nDORMITORIO`)/viviendas_2$Total
viviendas_2$Porcentaje_2<-viviendas_2$`2 \nDORMITORIOS`/viviendas_2$Total
viviendas_2$Porcentaje_3<-viviendas_2$`3 \nDORMITORIOS`/viviendas_2$Total
viviendas_2$Porcentaje_4<-viviendas_2$`4 \nDORMITORIOS`/viviendas_2$Total
viviendas_2$Porcentaje_5<-viviendas_2$`5 \nDORMITORIOS`/viviendas_2$Total
viviendas_2$Porcentaje_6<-viviendas_2$`6 O MÁS DORMITORIOS `/viviendas_2$Total

viviendas_2$`0 \nDORMITORIOS`<-NULL
viviendas_2$`1 \nDORMITORIO`<-NULL
viviendas_2$`2 \nDORMITORIOS`<-NULL
viviendas_2$`3 \nDORMITORIOS`<-NULL
viviendas_2$`4 \nDORMITORIOS`<-NULL
viviendas_2$`5 \nDORMITORIOS`<-NULL
viviendas_2$`6 O MÁS DORMITORIOS `<-NULL
viviendas_2$`CANTIDAD DE PERSONAS POR VIVIENDA`<-NULL
viviendas_2$`TOTAL VIVIENDAS PARTICULARES CON MORADORES PRESENTES`<-NULL
viviendas_2$`CANTIDAD DE DORMITORIOS IGNORADOS`<-NULL
viviendas_2$Total<-NULL

viviendas_3<-left_join(viviendas,viviendas_2)

viviendas_3$"Número de dormitorios"<-viviendas_3$Porcentaje_1*viviendas_3$Total*1+
  viviendas_3$Porcentaje_2*viviendas_3$Total*2+
  viviendas_3$Porcentaje_3*viviendas_3$Total*3+
  viviendas_3$Porcentaje_4*viviendas_3$Total*4+
  viviendas_3$Porcentaje_5*viviendas_3$Total*5+
  viviendas_3$Porcentaje_6*viviendas_3$Total*6
  viviendas_3$"Número de dormitorios"<-round(viviendas_3$"Número de dormitorios",0)
  viviendas_3$`NOMBRE COMUNA`<-as.character(viviendas_3$`NOMBRE COMUNA`)
  viviendas_3$Total<-NULL
  viviendas_3$Porcentaje_1<-NULL
  viviendas_3$Porcentaje_2<-NULL
  viviendas_3$Porcentaje_3<-NULL
  viviendas_3$Porcentaje_4<-NULL
  viviendas_3$Porcentaje_5<-NULL
  viviendas_3$Porcentaje_6<-NULL
  viviendas_3$Total<-NULL

rm(viviendas_2)  
rm(viviendas)
  
  
poblacion_dormitorios<-select(avance_contagiados_chile,COMUNA,Población)
poblacion_dormitorios$COMUNA<-as.character(poblacion_dormitorios$COMUNA)
poblacion_dormitorios$Población<-as.character(poblacion_dormitorios$Población)
poblacion_dormitorios$comuna_1<-""
poblacion_dormitorios$comuna_1<-tolower(stri_trans_general(poblacion_dormitorios$COMUNA, "Latin-ASCII"))
poblacion_dormitorios<-unique(poblacion_dormitorios)


viviendas_3$comuna_1<-""
viviendas_3$comuna_1<-tolower(stri_trans_general(viviendas_3$`NOMBRE COMUNA`, "Latin-ASCII"))


poblacion_dormitorios<-left_join(viviendas_3,poblacion_dormitorios)
poblacion_dormitorios<-unique(na.omit(poblacion_dormitorios))




  