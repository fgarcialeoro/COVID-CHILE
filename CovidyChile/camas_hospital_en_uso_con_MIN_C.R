library(dplyr)
library(tidyr)

camas_en_uso_hospital<-function(){
  camas_en_uso_hospital<-read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto24/CamasHospital_Diario.csv",check.names = FALSE,header = TRUE)
  camas_en_uso_hospital<-gather(camas_en_uso_hospital,key="Fecha",value = "camas","2020-04-16":ncol(camas_en_uso_hospital))
  camas_en_uso_hospital[, ]<-lapply(camas_en_uso_hospital[, ], as.character)
  camas_en_uso_hospital$camas<-as.numeric(camas_en_uso_hospital$camas)
  camas_en_uso_hospital$Fecha<-as.Date(camas_en_uso_hospital$Fecha)
  camas_en_uso_hospital
  }

