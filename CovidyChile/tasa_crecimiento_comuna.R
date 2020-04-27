library(dplyr)
library(tidyr)

source("casos_confirmados_comuna_con_MIN_C.R")

tasa_crecimiento<-function(){


casos_confirmados_comuna<-casos_confirmados_comuna()

comunas<-unique(casos_confirmados_comuna$Comuna)

estimacion<-c()
estimacion_i<-c()


for (i in (1:length(comunas))){
  
  comuna_i<-filter(casos_confirmados_comuna,Comuna==comunas[i])
  
  comuna_i<-filter(comuna_i,Fecha>=as.Date(max(comuna_i$Fecha)-9))
  
  
  try({
    
    if(comuna_i$`Casos confirmados acumulados cada 100000 habitantes`[nrow(comuna_i)]==0 | max(comuna_i$`Casos confirmados acumulados cada 100000 habitantes`) > comuna_i$`Casos confirmados acumulados cada 100000 habitantes`[nrow(comuna_i)] ){
      
      estimacion_i$Fecha<-max(comuna_i$Fecha)
      estimacion_i<-as.data.frame(estimacion_i)
      estimacion_i$Region<-comuna_i$Region[1]
      estimacion_i$Comuna<-comuna_i$Comuna[1]
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
      
      model<-lm(log(`Casos confirmados acumulados cada 100000 habitantes`)~Fecha,data=comuna_i)
      summary(model)[["coefficients"]]
      plot(log(comuna_i$`Casos confirmados acumulados cada 100000 habitantes`)~Fecha,comuna_i)
      estimacion_i$Fecha<-max(comuna_i$Fecha)
      estimacion_i<-as.data.frame(estimacion_i)
      estimacion_i$Region<-comuna_i$Region[1]
      estimacion_i$Comuna<-comuna_i$Comuna[1]
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


tasa_crecimiento<-select(casos_confirmados_comuna,Fecha,Region,`Codigo region`, Comuna,`Codigo comuna`,Poblacion,`Casos confirmados acumulados`,`Casos confirmados acumulados cada 100000 habitantes`)
tasa_crecimiento<-na.omit(left_join(estimacion,tasa_crecimiento))

tasa_crecimiento$Intercepto<-NULL
tasa_crecimiento$u_Intercepto<-NULL
tasa_crecimiento$u_Tasa<-NULL
tasa_crecimiento$n<-NULL
tasa_crecimiento$gl<-NULL
tasa_crecimiento$factor_t<-NULL
tasa_crecimiento$`U(Tasa de crecimiento), 95%`<-NULL
tasa_crecimiento$`Tasa de crecimiento`<-NULL

tasa_crecimiento<-select(tasa_crecimiento,Region,Comuna,Poblacion,`Casos confirmados acumulados`,`Casos confirmados acumulados cada 100000 habitantes`,`Tasa de crecimiento/%`,`U(Tasa de crecimiento)/%, 95%`)
tasa_crecimiento$`Casos confirmados acumulados cada 100000 habitantes`<-round(tasa_crecimiento$`Casos confirmados acumulados cada 100000 habitantes`,2)
tasa_crecimiento$`Tasa de crecimiento/%`<-round(tasa_crecimiento$`Tasa de crecimiento/%`,2)
tasa_crecimiento$`U(Tasa de crecimiento)/%, 95%`<- round(tasa_crecimiento$`U(Tasa de crecimiento)/%, 95%`,2)
#tasa_crecimiento<-filter(tasa_crecimiento,`Tasa de crecimiento/%`>0)
tasa_crecimiento<-unique(tasa_crecimiento)
tasa_crecimiento<-arrange(tasa_crecimiento, desc(`Tasa de crecimiento/%`))
tasa_crecimiento}
