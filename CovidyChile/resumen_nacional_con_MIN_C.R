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



resumen_nacional<-function(){
Poblacion<-19458310
avance_todo_chile<-read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales.csv",header=T,check.names=F)
avance_todo_chile<-gather(avance_todo_chile,key="Tipo de dato",value = "casos","2020-03-03":ncol(avance_todo_chile))
avance_todo_chile<-spread(avance_todo_chile,Fecha,casos)
colnames(avance_todo_chile)[which(names(avance_todo_chile) == "Tipo de dato")] <- "Fecha"
avance_todo_chile$"Casos totales cada 100000 habitantes"<-100000*avance_todo_chile$`Casos totales`/Poblacion
avance_todo_chile$Fecha<-as.Date(avance_todo_chile$Fecha)
avance_todo_chile$`Casos totales cada 100000 habitantes`<-as.numeric(avance_todo_chile$`Casos totales cada 100000 habitantes`)



tasa<-c()
tasa_i<-c()
dias<-avance_todo_chile$Fecha

for (i in 3:length(dias)){
  
  if(i<=10){
    avance_todo_chile_i<-filter(avance_todo_chile,Fecha<=dias[i])
    model<-lm(log(`Casos totales cada 100000 habitantes`)~Fecha,avance_todo_chile_i)
    summary(model)
    tasa_i$Fecha<-dias[i]
    tasa_i<-as.data.frame(tasa_i)
    tasa_i$"Tasa de crecimiento"<-as.numeric(summary(model)[["coefficients"]][2,1])
    tasa_i$"u(Tasa de crecimiento)"<-as.numeric(summary(model)[["coefficients"]][2,2])
    tasa_i$n<-nrow(avance_todo_chile_i)
    tasa_i$t<-qt(1-0.05/2,nrow(avance_todo_chile_i)-2)
    tasa_i$"U(Tasa de crecimiento), 95%"<-(tasa_i$"u(Tasa de crecimiento)"/sqrt(tasa_i$n))*tasa_i$t
    tasa<-rbind(tasa_i,tasa)
  }else{
    avance_todo_chile_i<-filter(avance_todo_chile,Fecha<=dias[i] & Fecha>=dias[i-4])
    model<-lm(log(`Casos totales cada 100000 habitantes`)~Fecha,avance_todo_chile_i)
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
  
  
}

tasa$`Tasa de crecimiento /%`<-tasa$`Tasa de crecimiento`*100
tasa$`U(Tasa de crecimiento)/%, 95%`<-tasa$`U(Tasa de crecimiento), 95%`*100
tasa$`Tasa de crecimiento`<-NULL
tasa$`u(Tasa de crecimiento)`<-NULL
tasa$`U(Tasa de crecimiento), 95%`<-NULL

tasa<-left_join(tasa,avance_todo_chile)







tasa_rec<-c()
tasa_rec_i<-c()
dias<-avance_todo_chile$Fecha

for (i in 17:length(dias)){

  if(i<=25){
    avance_todo_chile_i<-filter(avance_todo_chile,Fecha<=dias[i] & Fecha>dias[14])
    model<-lm(log(`Casos recuperados`)~Fecha,avance_todo_chile_i)
    summary(model)
    tasa_rec_i$Fecha<-dias[i]
    tasa_rec_i<-as.data.frame(tasa_rec_i)
    tasa_rec_i$"tasa_rec de crecimiento"<-as.numeric(summary(model)[["coefficients"]][2,1])
    tasa_rec_i$"u(tasa_rec de crecimiento)"<-as.numeric(summary(model)[["coefficients"]][2,2])
    tasa_rec_i$n<-nrow(avance_todo_chile_i)
    tasa_rec_i$t<-qt(1-0.05/2,nrow(avance_todo_chile_i)-2)
    tasa_rec_i$"U(tasa_rec de crecimiento), 95%"<-(tasa_rec_i$"u(tasa_rec de crecimiento)"/sqrt(tasa_rec_i$n))*tasa_rec_i$t
    tasa_rec<-rbind(tasa_rec_i,tasa_rec)
  }else{
    avance_todo_chile_i<-filter(avance_todo_chile,Fecha<=dias[i] & Fecha>=dias[i-4])
    model<-lm(log(`Casos recuperados`)~Fecha,avance_todo_chile_i)
    summary(model)
    tasa_rec_i$Fecha<-dias[i]
    tasa_rec_i<-as.data.frame(tasa_rec_i)
    tasa_rec_i$"tasa_rec de crecimiento"<-as.numeric(summary(model)[["coefficients"]][2,1])
    tasa_rec_i$"u(tasa_rec de crecimiento)"<-as.numeric(summary(model)[["coefficients"]][2,2])
    tasa_rec_i$n<-nrow(avance_todo_chile_i)
    tasa_rec_i$t<-qt(1-0.05/2,nrow(avance_todo_chile_i)-2)
    tasa_rec_i$"U(tasa_rec de crecimiento), 95%"<-(tasa_rec_i$"u(tasa_rec de crecimiento)"/sqrt(tasa_rec_i$n))*tasa_rec_i$t
    tasa_rec<-rbind(tasa_rec_i,tasa_rec)
    print(i)
  }
  
  
}

tasa_rec$`tasa_rec de crecimiento /%`<-tasa_rec$`tasa_rec de crecimiento`*100
tasa_rec$`U(tasa_rec de crecimiento)/%, 95%`<-tasa_rec$`U(tasa_rec de crecimiento), 95%`*100
tasa_rec$`tasa_rec de crecimiento`<-NULL
tasa_rec$`u(tasa_rec de crecimiento)`<-NULL
tasa_rec$`U(tasa_rec de crecimiento), 95%`<-NULL

tasa<-left_join(tasa,tasa_rec)

tasa$"Casos activos cada 100000 habitantes"<-100000*tasa$`Casos activos`/Poblacion
tasa$"Casos nuevos cada 100000 habitantes"<-100000*tasa$`Casos nuevos`/Poblacion
tasa$"Casos recuperados cada 100000 habitantes"<-100000*tasa$`Casos recuperados`/Poblacion
tasa$"Casos fallecidos cada 100000 habitantes"<-100000*tasa$Fallecidos/Poblacion


tasa
}

