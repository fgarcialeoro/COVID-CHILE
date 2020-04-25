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





avance_contagiados_chile<-read.xlsx("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/datos_minsal.xlsx",sheetName ="avance_contagiados_comuna")
avance_contagiados_chile$Fecha<-as.Date(as.character(avance_contagiados_chile$Fecha))
avance_contagiados_chile$REGION<-as.character(avance_contagiados_chile$REGION)
avance_contagiados_chile$COMUNA<-as.character(avance_contagiados_chile$COMUNA)
avance_contagiados_chile$Población<-as.numeric(as.character(avance_contagiados_chile$Población))
avance_contagiados_chile$Tasa.incidencia<-as.numeric(as.character(avance_contagiados_chile$Tasa.incidencia))
avance_contagiados_chile$Confirmados<-as.numeric(as.character(avance_contagiados_chile$Confirmados))

avance_contagiados_chile_confirmados_NA<-filter(avance_contagiados_chile,is.na(Confirmados))
avance_contagiados_chile_confirmados_NA$Confirmados<-round((avance_contagiados_chile_confirmados_NA$Tasa.incidencia*avance_contagiados_chile_confirmados_NA$Población)/100000,0)
avance_contagiados_chile_confirmados_no_NA<-filter(avance_contagiados_chile,!is.na(Confirmados))
avance_contagiados_chile<-rbind(avance_contagiados_chile_confirmados_NA,avance_contagiados_chile_confirmados_no_NA)
rm(avance_contagiados_chile_confirmados_NA,avance_contagiados_chile_confirmados_no_NA)

avance_contagiados_chile$"Contagiados cada 100000 habitantes"<-(avance_contagiados_chile$Confirmados/avance_contagiados_chile$Población)*100000
avance_contagiados_chile$COMUNA<-gsub("[[:space:]]*$","",avance_contagiados_chile$COMUNA)###ver ?regex
avance_contagiados_chile$REGION<-gsub("[[:space:]]*$","",avance_contagiados_chile$REGION)###ver ?regex
avance_contagiados_chile<-unique(arrange(avance_contagiados_chile,Fecha))

#avance_contagiados_chile$REGION<-iconv(avance_contagiados_chile$REGION,from="UTF-8",to="ASCII//TRANSLIT")

write.csv(avance_contagiados_chile,"www/avance_contagiados_chile.csv",row.names = FALSE)





################################################################################################
##############################Mapas por comuna##################################################
################################################################################################

divisiones_administrativas<- readOGR("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/DivisionPoliticoAdministrativa2019/DivisionPoliticaAdministrativa2019.shp", GDAL1_integer64_policy = TRUE)
divisiones <- spTransform(divisiones_administrativas, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))

regiones<-unique(divisiones$REGION)

mapa<-subset(divisiones, divisiones$REGION==regiones[1])
mapa<-ms_simplify(mapa, keep = 0.001)

for (i in c(2:10,13:16)){
print(i)
region_i<-subset(divisiones, divisiones$REGION==regiones[i])
region_i<-ms_simplify(region_i, keep = 0.001)
mapa<-rbind(region_i,mapa)}

region_11<-subset(divisiones, divisiones$REGION==regiones[11])
#region_11<-ms_simplify(region_11, keep = 0.001)
region_12<-subset(divisiones, divisiones$REGION==regiones[12])
#region_12<-ms_simplify(region_12, keep = 0.001)


mapa<-rbind(region_11,mapa)
mapa<-rbind(region_12,mapa)


mapa@data$COMUNA<-gsub("Los Alamos","Los Álamos",mapa@data$COMUNA,fixed = TRUE)
mapa@data$COMUNA<-gsub("Los Angeles","Los Ángeles",mapa@data$COMUNA,fixed = TRUE)


fechas<-unique(avance_contagiados_chile$Fecha)
regiones<-unique(avance_contagiados_chile$REGION)
for (i in (1:length(fechas))){
  avance_contagiados_chile_i<-subset(avance_contagiados_chile, avance_contagiados_chile$Fecha==fechas[i])
  
  for (j in (1:length(regiones))){
    avance_contagiados_chile_i_j<-subset(avance_contagiados_chile_i, avance_contagiados_chile_i$REGION==regiones[j])
    mapa_j<-subset(mapa, mapa$REGION==regiones[j])
    mapa_i_j<-mapa_j
    mapa_i_j@data<-left_join(mapa_j@data,avance_contagiados_chile_i_j,by=c("COMUNA","REGION"))
    saveRDS(mapa_i_j,paste("www/mapas/mapa_chile",toupper(stri_trans_general(regiones[j], "Latin-ASCII")),fechas[i],sep="-"))
    }
  }
rm(mapa,mapa_i_j,region_11,region_12,region_i,mapa_j,avance_contagiados_chile_i,avance_contagiados_chile_i_j,divisiones,divisiones_administrativas)




################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
######################################ajuste de datos para estimar tasa de crecimiento##########################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
   
comunas<-unique(avance_contagiados_chile$COMUNA)

estimacion<-c()
estimacion_i<-c()


for (i in (1:length(comunas))){

  comuna_i<-filter(avance_contagiados_chile,COMUNA==comunas[i])
  comuna_i<-filter(comuna_i,`Contagiados cada 100000 habitantes`>0)
  
  
try({
  
  if(comuna_i$`Contagiados cada 100000 habitantes`[nrow(comuna_i)]==0 | max(comuna_i$`Contagiados cada 100000 habitantes`) > comuna_i$`Contagiados cada 100000 habitantes`[nrow(comuna_i)] ){
  
    estimacion_i$Fecha<-max(comuna_i$Fecha)
    estimacion_i<-as.data.frame(estimacion_i)
    estimacion_i$REGION<-comuna_i$REGION[1]
    estimacion_i$COMUNA<-comuna_i$COMUNA[1]
    estimacion_i$Intercepto<-NA
    estimacion_i$u_Intercepto<-NA
    estimacion_i$"Tasa de crecimiento"<-NA
    estimacion_i$u_Tasa<-NA
    estimacion_i$n<-NA
    estimacion_i$gl<-NA
    estimacion_i$factor_t<<-NA
    estimacion_i$"U(Tasa de crecimiento), 95%"<-NA
  }
  else{
    
    model<-lm(log(`Contagiados cada 100000 habitantes`)~Fecha,data=comuna_i)
    summary(model)[["coefficients"]]
    plot(log(comuna_i$`Contagiados cada 100000 habitantes`)~Fecha,comuna_i)
    estimacion_i$Fecha<-max(comuna_i$Fecha)
    estimacion_i<-as.data.frame(estimacion_i)
    estimacion_i$REGION<-comuna_i$REGION[1]
    estimacion_i$COMUNA<-comuna_i$COMUNA[1]
    estimacion_i$Intercepto<-summary(model)[["coefficients"]][1,1]
    estimacion_i$u_Intercepto<-summary(model)[["coefficients"]][1,2]
    estimacion_i$"Tasa de crecimiento"<-summary(model)[["coefficients"]][2,1]
    estimacion_i$u_Tasa<-summary(model)[["coefficients"]][2,2]
    estimacion_i$n<-nrow(comuna_i)
    estimacion_i$gl<-nrow(comuna_i)-2
    estimacion_i$factor_t<-qt(1-0.05/2, estimacion_i$gl)
    estimacion_i$"U(Tasa de crecimiento), 95%"<-(estimacion_i$u_Tasa/sqrt(estimacion_i$n))*estimacion_i$factor_t
    estimacion_i$"Tasa de crecimiento/%"<-estimacion_i$"Tasa de crecimiento"*100
    estimacion_i$"U(Tasa de crecimiento)/%, 95%"<-estimacion_i$"U(Tasa de crecimiento), 95%"*100
  }}, silent=TRUE)
  
estimacion<-rbind(estimacion_i,estimacion)
rm(model)
print(i)
} 
rm(comuna_i,estimacion_i)


tasa_crecimiento<-select(avance_contagiados_chile,Fecha,REGION, COMUNA,"Población",`Contagiados cada 100000 habitantes`)
tasa_crecimiento<-na.omit(left_join(estimacion,tasa_crecimiento))

tasa_crecimiento$Intercepto<-NULL
tasa_crecimiento$u_Intercepto<-NULL
tasa_crecimiento$u_Tasa<-NULL
tasa_crecimiento$n<-NULL
tasa_crecimiento$gl<-NULL
tasa_crecimiento$factor_t<-NULL
tasa_crecimiento$`U(Tasa de crecimiento), 95%`<-NULL
tasa_crecimiento$`Tasa de crecimiento`<-NULL

tasa_crecimiento<-select(tasa_crecimiento,REGION,COMUNA,Población,`Contagiados cada 100000 habitantes`,`Tasa de crecimiento/%`,`U(Tasa de crecimiento)/%, 95%`)
tasa_crecimiento$`Contagiados cada 100000 habitantes`<-round(tasa_crecimiento$`Contagiados cada 100000 habitantes`,1)
tasa_crecimiento$`Tasa de crecimiento/%`<-round(tasa_crecimiento$`Tasa de crecimiento/%`,1)
tasa_crecimiento$`U(Tasa de crecimiento)/%, 95%`<- round(tasa_crecimiento$`U(Tasa de crecimiento)/%, 95%`,1)
tasa_crecimiento<-filter(tasa_crecimiento,`Tasa de crecimiento/%`>0)
tasa_crecimiento<-unique(tasa_crecimiento)
tasa_crecimiento<-arrange(tasa_crecimiento, desc(`Tasa de crecimiento/%`))


write.csv(tasa_crecimiento,"www/tasa_crecimiento.csv",row.names = FALSE)





##https://data-flair.training/blogs/r-nonlinear-regression/
##Asym+(R0-Asym)*exp(-exp(lrc)*input)
##apropos("^SS")
####################################################################################################
####################################################################################################
####################################################################################################
#####################XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX############################
#####################XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX############################
#####################XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX############################
#####################XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX############################
#####################XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX############################
#####################XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX############################
#####################XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX############################
#####################XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX############################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################Lo mismo, pero con los datos de MIN_CyT
avance_contagiados_chile<-read.csv("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/Datos-COVID19/output/producto1/Covid-19.csv",check.names = FALSE,header = TRUE)
avance_contagiados_chile<-gather(avance_contagiados_chile,key="Fecha",value = "casos","2020-03-30":(ncol(avance_contagiados_chile)-1))
avance_contagiados_chile[, ]<-lapply(avance_contagiados_chile[, ], as.character)
avance_contagiados_chile$Poblacion<-as.numeric(avance_contagiados_chile$Poblacion)
avance_contagiados_chile$casos<-as.numeric(avance_contagiados_chile$casos)
avance_contagiados_chile$Fecha<-as.Date(avance_contagiados_chile$Fecha)
avance_contagiados_chile$`Codigo comuna`<-as.numeric(avance_contagiados_chile$`Codigo comuna`)
avance_contagiados_chile$`Codigo region`<-as.numeric(avance_contagiados_chile$`Codigo region`)
################################################################################################
##############################Mapas por comuna##################################################


divisiones_administrativas<- readOGR("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/DivisionPoliticoAdministrativa2019/DivisionPoliticaAdministrativa2019.shp", GDAL1_integer64_policy = TRUE)
divisiones <- spTransform(divisiones_administrativas, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))

regiones<-as.character(unique(divisiones$CUT_REG))

mapa<-subset(divisiones, divisiones$CUT_REG==regiones[1])
mapa<-ms_simplify(mapa, keep = 0.001)

for (i in c(2:10,13:16)){
  print(i)
  region_i<-subset(divisiones, divisiones$CUT_REG==regiones[i])
  region_i<-ms_simplify(region_i, keep = 0.001)
  mapa<-rbind(region_i,mapa)}

region_11<-subset(divisiones, divisiones$CUT_REG==regiones[11])
#region_11<-ms_simplify(region_11, keep = 0.001)
region_12<-subset(divisiones, divisiones$CUT_REG==regiones[12])
#region_12<-ms_simplify(region_12, keep = 0.001)


mapa<-rbind(region_11,mapa)
mapa<-rbind(region_12,mapa)

mapa@data$CUT_COM<-as.numeric(as.character(mapa@data$CUT_COM))
mapa@data$CUT_REG<-as.numeric(mapa@data$CUT_REG)
#mapa@data$COMUNA<-gsub("Los Alamos","Los Álamos",mapa@data$COMUNA,fixed = TRUE)
#mapa@data$COMUNA<-gsub("Los Angeles","Los Ángeles",mapa@data$COMUNA,fixed = TRUE)


fechas<-unique(avance_contagiados_chile$Fecha)
regiones<-unique(avance_contagiados_chile$`Codigo region`)

for (i in (1:length(fechas))){
  avance_contagiados_chile_i<-subset(avance_contagiados_chile, avance_contagiados_chile$Fecha==fechas[i])
  j<-1
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




  