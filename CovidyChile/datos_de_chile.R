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




########################################################################################################################
###############estimación a nivel país de variacion de tasa####################################################
########################################################################################################
avance_todo_chile<-read.xlsx("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/datos_minsal.xlsx",sheetName ="Avance_nacional")
colnames(avance_todo_chile)[which(names(avance_todo_chile) == "Casos.confirmados.cada.100000.habitantes")] <- "Casos confirmados acumulados cada 100000 habitantes"
colnames(avance_todo_chile)[which(names(avance_todo_chile) == "Confirmados.acumulados")] <- "Casos confirmados acumulados"
write.csv(avance_todo_chile,"www/avance_todo_chile.csv",row.names = FALSE)

plot(log(`Casos confirmados acumulados cada 100000 habitantes`)~Fecha,data=avance_todo_chile) 
model<-lm(log(`Casos confirmados acumulados cada 100000 habitantes`)~Fecha,avance_todo_chile)
summary(model)[["coefficients"]]

##Variacion de tasa por día.

tasa<-c()
tasa_i<-c()
dias<-avance_todo_chile$Fecha

for (i in 3:length(dias)){
  avance_todo_chile_i<-filter(avance_todo_chile,Fecha<=dias[i])
  model<-lm(log(`Casos confirmados acumulados cada 100000 habitantes`)~Fecha,avance_todo_chile_i)
  summary(model)
  tasa_i$Fecha<-dias[i]
  tasa_i<-as.data.frame(tasa_i)
  tasa_i$"Tasa de crecimiento"<-as.numeric(summary(model)[["coefficients"]][2,1])
  tasa_i$"u(Tasa de crecimiento)"<-as.numeric(summary(model)[["coefficients"]][2,2])
  tasa_i$n<-nrow(avance_todo_chile_i)
  tasa_i$t<-qt(1-0.05/2,nrow(avance_todo_chile_i)-2)
  tasa_i$"U(Tasa de crecimiento), 95%"<-(tasa_i$"u(Tasa de crecimiento)"/sqrt(tasa_i$n))*tasa_i$t
  tasa<-rbind(tasa_i,tasa)
  }

tasa$`Tasa de crecimiento /%`<-tasa$`Tasa de crecimiento`*100
tasa$`U(Tasa de crecimiento)/%, 95%`<-tasa$`U(Tasa de crecimiento), 95%`*100
tasa$`Tasa de crecimiento`<-NULL
tasa$`u(Tasa de crecimiento)`<-NULL
tasa$`U(Tasa de crecimiento), 95%`<-NULL
write.csv(tasa,"www/tasa_crecimiento_nacional.csv",row.names = FALSE)






######Mapas
divisiones_administrativas<- readOGR("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/CovidyChile/www/DivisionPoliticoAdministrativa2019/DivisionPoliticaAdministrativa2019.shp", GDAL1_integer64_policy = TRUE)
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
region_11<-ms_simplify(region_11, keep = 0.001)
region_12<-subset(divisiones, divisiones$REGION==regiones[12])
region_12<-ms_simplify(region_12, keep = 0.001)


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
 


#for (i in (1:length(fechas))){
#  avance_contagiados_chile_i<-subset(avance_contagiados_chile, avance_contagiados_chile$Fecha==fechas[i])
# mapa_i<-mapa
# mapa_i@data<-left_join(mapa_i@data,avance_contagiados_chile_i,by=c("COMUNA","REGION"))
# saveRDS(mapa_i,paste("www/mapas/mapa_chile",fechas[i],sep="-"))
# }


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

for (i in (1:length(comunas))){
i<-1
  comuna_i<-filter(avance_contagiados_chile,COMUNA==comunas[i])
  comuna_i<-filter(comuna_i,`Contagiados cada 100000 habitantes`>0)
  
try({
  
  if(comuna_i$`Contagiados cada 100000 habitantes`[nrow(comuna_i)]==0 | max(comuna_i$`Contagiados cada 100000 habitantes`) > comuna_i$`Contagiados cada 100000 habitantes`[nrow(comuna_i)] ){
  
  estimacion_i$Fecha<-max(comuna_i$Fecha)
  estimacion_i$REGION<-comuna_i$REGION[1]
  estimacion_i$COMUNA<-comuna_i$COMUNA[1]
  estimacion_i$Intercepto<-NA
  estimacion_i$u_Intercepto<-NA
  estimacion_i$Tasa<-NA
  estimacion_i$u_Tasa<-NA
  estimacion_i<-as.data.frame(estimacion_i)}
  else{
    
    plot(`Contagiados cada 100000 habitantes`~ as.numeric(Fecha),data=comuna_i)
    model<-lm(log(`Contagiados cada 100000 habitantes`)~as.numeric(Fecha),data=comuna_i)
    summary(model)[["coefficients"]]
    
    estimacion_i$Fecha<-max(comuna_i$Fecha)
    estimacion_i$REGION<-comuna_i$REGION[1]
    estimacion_i$COMUNA<-comuna_i$COMUNA[1]
    estimacion_i$Intercepto<-summary(model)[["coefficients"]][1,1]
    estimacion_i$u_Intercepto<-summary(model)[["coefficients"]][1,2]
    estimacion_i$Tasa<-summary(model)[["coefficients"]][2,1]
    estimacion_i$u_Tasa<-summary(model)[["coefficients"]][2,2]
    estimacion_i$n<-nrow(estimacion_i)
    estimacion_i$gl<-nrow(estimacion_i)-2
    estimacion_i$factor_t<-qt(1-0.05/2, estimacion_i$gl)
    
  }}, silent=TRUE)
  
estimacion<-rbind(estimacion_i,estimacion)
rm(model)
print(i)
} 


tasa_crecimiento<-select(avance_contagiados_chile,Fecha,REGION, COMUNA,"Población",`Contagiados cada 100000 habitantes`)
tasa_crecimiento<-na.omit(left_join(estimacion,tasa_crecimiento))

tasa_crecimiento$Intercepto<-NULL
tasa_crecimiento$u_Intercepto<-NULL
tasa_crecimiento<-select(tasa_crecimiento,Fecha,REGION,COMUNA,Población,`Contagiados cada 100000 habitantes`,
                         Tasa,u_Tasa)

tasa_crecimiento$"Tasa de crecimiento / %"<-round(100*tasa_crecimiento$Tasa,1)
tasa_crecimiento$"U(Tasa de crecimiento / %, 95%)"<-round(100*tasa_crecimiento$u_Tasa*factor_t/sqrt(grados_de_libertad+2),1)
tasa_crecimiento$Tasa<-NULL
tasa_crecimiento$u_Tasa<-NULL
tasa_crecimiento$`Contagiados cada 100000 habitantes`<-round(tasa_crecimiento$`Contagiados cada 100000 habitantes`,1)
tasa_crecimiento<-arrange(tasa_crecimiento, desc(`Tasa de crecimiento / %`))
tasa_crecimiento<-filter(tasa_crecimiento,`Tasa de crecimiento / %`>0)
tasa_crecimiento<-unique(tasa_crecimiento)

write.csv(tasa_crecimiento,"www/tasa_crecimiento.csv",row.names = FALSE)





##https://data-flair.training/blogs/r-nonlinear-regression/
##Asym+(R0-Asym)*exp(-exp(lrc)*input)
##apropos("^SS")













