library(dplyr)
library(tidyr)

casos_confirmados_comuna<-function(){
  casos_confirmados_comuna<-read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19.csv",check.names = FALSE,header = TRUE)
  casos_confirmados_comuna<-gather(casos_confirmados_comuna,key="Fecha",value = "casos","2020-03-30":(ncol(casos_confirmados_comuna)-1))
  casos_confirmados_comuna[, ]<-lapply(casos_confirmados_comuna[, ], as.character)
  casos_confirmados_comuna$Poblacion<-as.numeric(casos_confirmados_comuna$Poblacion)
  casos_confirmados_comuna$casos<-as.numeric(casos_confirmados_comuna$casos)
  casos_confirmados_comuna$Fecha<-as.Date(casos_confirmados_comuna$Fecha)
  casos_confirmados_comuna$`Codigo comuna`<-as.numeric(casos_confirmados_comuna$`Codigo comuna`)
  casos_confirmados_comuna$`Codigo region`<-as.numeric(casos_confirmados_comuna$`Codigo region`)
  casos_confirmados_comuna$Poblacion<-as.numeric(casos_confirmados_comuna$Poblacion)
  colnames(casos_confirmados_comuna)[which(names(casos_confirmados_comuna) == "casos")] <- "Casos confirmados acumulados"
  casos_confirmados_comuna$"Casos confirmados acumulados cada 100000 habitantes"<-(casos_confirmados_comuna$`Casos confirmados acumulados`/casos_confirmados_comuna$Poblacion)*100000
  casos_confirmados_comuna
  }

