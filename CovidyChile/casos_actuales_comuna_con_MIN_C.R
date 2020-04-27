library(dplyr)
library(tidyr)

casos_actuales_comuna<-function(){
casos_actuales_comuna<-read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto25/CasosActualesPorComuna.csv",header=T,check.names=F)
casos_actuales_comuna<-filter(casos_actuales_comuna,Comuna!="Total")
casos_actuales_comuna<-gather(casos_actuales_comuna,key="Fecha",value = "Casos actuales","2020-04-13":(ncol(casos_actuales_comuna)))
casos_actuales_comuna[, ]<-lapply(casos_actuales_comuna[, ],as.character)
casos_actuales_comuna$Poblacion<-as.numeric(casos_actuales_comuna$Poblacion)
casos_actuales_comuna$`Casos actuales`<-as.numeric(casos_actuales_comuna$`Casos actuales`)
casos_actuales_comuna$Fecha<-as.Date(casos_actuales_comuna$Fecha)
casos_actuales_comuna$`Codigo comuna`<-as.numeric(casos_actuales_comuna$`Codigo comuna`)
casos_actuales_comuna$`Codigo region`<-as.numeric(casos_actuales_comuna$`Codigo region`)
casos_actuales_comuna$"Casos actuales cada 100000 habitantes"<-(casos_actuales_comuna$`Casos actuales`/casos_actuales_comuna$Poblacion)*100000
casos_actuales_comuna
}

