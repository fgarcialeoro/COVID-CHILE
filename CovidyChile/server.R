library(plotly)
library(xlsxjars)
library(xlsx)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(lubridate)
library(googleVis)
library(shiny)
library(rgdal)
library(leaflet)
library(sp)
library(sf)
library(shinyWidgets)
library(rmapshaper)
library(stringr)
library(stringi)
library(GGally)
library(DT)
library(scales)

Verde_oscuro<-rgb(73,79,14,maxColorValue=255)
Azul_claro<-rgb(0,71,130, maxColorValue=255)
Rojo<-rgb(143,19,14, maxColorValue=255)
Verde_claro<-rgb(198,206,6,maxColorValue=255)

source("resumen_nacional_con_MIN_C.R")
source("casos_actuales_comuna_con_MIN_C.R")
source("casos_confirmados_comuna_con_MIN_C.R")
source("tasa_crecimiento_comuna.R")
source("mapa_actuales_con_MIN_C.R")
source("indicadores_internacionales.R")
source("camas_hospital_en_uso_con_MIN_C.R")

server <- function(session,input, output) {

 
url <- a("www.commonsense.cl",href="https://www.commonsense.cl")
output$tab <- renderUI({url})
output$tab1 <- renderUI({url})
output$tab3 <- renderUI({url})
output$tab4 <- renderUI({url})
  ######################################################3
  
####tabPanel("Casos actuales y comunas", 
casos_actuales_comuna<-casos_actuales_comuna()
comunas_actuales<-unique(casos_actuales_comuna$Region)
fechas_actuales<-unique(casos_actuales_comuna$Fecha)


output$comunas_cada_100_mil<-renderPlotly({
  
  casos_actuales_comuna<-casos_actuales_comuna()
  casos_actuales_comuna$"Region-Comuna"<-paste(casos_actuales_comuna$Region,casos_actuales_comuna$Comuna,sep="-")  
  
    if(input$i_ESCALAY =="Lineal"){escala<-scale_y_continuous()} else {escala<-scale_y_log10()}
  
  
  m<-ggplot(filter(casos_actuales_comuna,`Region-Comuna` %in% input$i_REGION_COMUNA) , aes(Fecha,`Casos actuales cada 100000 habitantes`,color = `Region-Comuna`))+geom_point()
  m<-m+ggtitle("Histórico casos actuales cada 100000 habitantes")+escala
  m<-ggplotly(m)
  m
}) 


output$comunas_absoluto<-renderPlotly({
  
  casos_actuales_comuna<-casos_actuales_comuna()
  casos_actuales_comuna$"Region-Comuna"<-paste(casos_actuales_comuna$Region,casos_actuales_comuna$Comuna,sep="-")  
  
  if(input$i_ESCALAY =="Lineal"){escala<-scale_y_continuous()} else {escala<-scale_y_log10()}
  
  
  m<-ggplot(filter(casos_actuales_comuna,`Region-Comuna` %in% input$i_REGION_COMUNA) , aes(Fecha,`Casos actuales`,color = `Region-Comuna`))+geom_point()
  m<-m+ggtitle("Histórico casos actuales")+escala
  m<-ggplotly(m)
  m
}) 

#output$barras_actuales<-renderPlot({
#  
# casos_actuales_comuna<-casos_actuales_comuna()
# casos_actuales_comuna$"Region-Comuna"<-paste(casos_actuales_comuna$Region,casos_actuales_comuna$Comuna,sep="-")  
#   m<-ggplot(filter(casos_actuales_comuna,Fecha==max(casos_actuales_comuna$Fecha)), aes(x = reorder(`Region-Comuna`,-`Casos actuales`),`Casos actuales`))+geom_col()+facet_wrap(~Region)
# m<-m+theme(axis.text.x = element_text(angle = 90, hjust = 1,size=1))
#   m<-m+ggtitle("Casos actuales por comuna")
# m<-ggplotly(m)
# m
#}) 




output$crecimiento<-renderDataTable({DT::datatable(tasa_crecimiento())})







##########tabPanel("Resumen nacional",

output$var_tasa<-renderPlotly({
  
  tasa_crecimiento_nacional<-resumen_nacional()
  tasa_crecimiento_nacional$Fecha<-as.Date(tasa_crecimiento_nacional$Fecha)
  
  r<-ggplot(tasa_crecimiento_nacional,aes(Fecha,`Tasa de crecimiento /%`))+geom_line(color=Rojo)
  r<-r+scale_y_continuous(breaks=seq(0, 70, 5))
  r<-r+scale_x_date(breaks="weeks")
  r<-r+ggtitle("Tasa de crecimiento de casos totales vs tiempo")
  r<-r+theme(axis.text.x = element_text(angle = 90))
  r<-ggplotly(r)
  r
}) 




output$todos<-renderPlotly({
  
  tasa_crecimiento_nacional<-resumen_nacional()
  tasa_crecimiento_nacional$Fecha<-as.Date(tasa_crecimiento_nacional$Fecha)
  tasa_crecimiento_nacional_2<-select(tasa_crecimiento_nacional,`Fecha`,`Casos activos`,`Casos nuevos`,`Casos recuperados`,Fallecidos,`Casos totales`)
  rm(tasa_crecimiento_nacional)
  
  
  tasa_crecimiento_nacional_2<-gather(tasa_crecimiento_nacional_2,key="Tipo de caso",value = "Casos",`Casos activos`,`Casos nuevos`,`Casos recuperados`,`Fallecidos`,`Casos totales`)
  tasa_crecimiento_nacional_2$`Tipo de caso`<-as.factor(tasa_crecimiento_nacional_2$`Tipo de caso`)
  z<-ggplot(tasa_crecimiento_nacional_2,aes(Fecha,Casos,col=`Tipo de caso`))+geom_point(size=1)
  z<-z+scale_y_continuous(breaks=seq(0, 50000, 1000))
  z<-z+scale_x_date(breaks="weeks")
  z<-z+ggtitle("Casos vs tiempo")+ theme(legend.position="bottom")
  z<-z+theme(axis.text.x = element_text(angle = 90))
  z<-ggplotly(z)
  z
}) 




output$todos_normalizado<-renderPlotly({
  
  tasa_crecimiento_nacional<-resumen_nacional()
  tasa_crecimiento_nacional$Fecha<-as.Date(tasa_crecimiento_nacional$Fecha)
  tasa_crecimiento_nacional_2<-select(tasa_crecimiento_nacional,`Fecha`,`Casos activos cada 100000 habitantes`,`Casos nuevos cada 100000 habitantes`,`Casos recuperados cada 100000 habitantes`,`Casos fallecidos cada 100000 habitantes`,`Casos totales cada 100000 habitantes`)
  rm(tasa_crecimiento_nacional)
  
  
  tasa_crecimiento_nacional_2<-gather(tasa_crecimiento_nacional_2,key="Tipo de caso",value = "Casos",`Casos activos cada 100000 habitantes`,`Casos nuevos cada 100000 habitantes`,`Casos recuperados cada 100000 habitantes`,`Casos fallecidos cada 100000 habitantes`,`Casos totales cada 100000 habitantes`)
  tasa_crecimiento_nacional_2$`Tipo de caso`<-as.factor(tasa_crecimiento_nacional_2$`Tipo de caso`)
  z<-ggplot(tasa_crecimiento_nacional_2,aes(Fecha,Casos,col=`Tipo de caso`))+geom_point(size=1)
  z<-z+scale_y_continuous(breaks=seq(0, 200, 5))+labs(y="Casos cada 100000 habitantes")
  z<-z+scale_x_date(breaks="weeks")
  z<-z+ggtitle("Casos cada 100000 habitantes vs tiempo")+ theme(legend.position="bottom")
  z<-z+theme(axis.text.x = element_text(angle = 90))
  z<-ggplotly(z)
  z
})


output$camas_en_uso<-renderPlotly({
  camas_en_uso_hospital<-camas_en_uso_hospital()
 z<-ggplot(camas_en_uso_hospital,aes(Fecha,camas,col=`Tipo de cama`))+geom_point(size=1)
 z<-z+scale_x_date(breaks="weeks")+labs(y="Número de camas")
 z<-z+ggtitle("Uso de camas hospitalarias")+ theme(legend.position="bottom")
 z<-ggplotly(z)
 z
 })





  
############tabPanel("Mundo: COVID y otros indicadores",

#  datos_paises<-indicadores_internacionales()
#  datos_paises$Date<-as.Date(as.character(datos_paises$Date)) 
 
#  output$distPlot<-renderGvis({gvisMotionChart(datos_paises,idvar="Country Name",timevar="Date",
#                                             xvar="Population above 65 [%]")})

#  cols <-character(nrow(datos_paises))
#  cols[] <-"black"
#  cols[datos_paises$`Science and Tech articles`==	7121.74] <-"red"
#  cols[datos_paises$`GDP per capita [USD]`==25222.53] <-"red"
#  cols[datos_paises$`Population above 65 [%]`==	23.01721] <-"red"
#  cols[datos_paises$`Government Effectiveness, percentil`==81.73077]<-"red"

#  output$matriz <- renderPlot(
#    pairs( ~log(`Deaths per 100000 inhabitants`)+log(`Science and Tech articles`)+log(`GDP per capita [USD]`)+`Population above 65 [%]`+(`Government Effectiveness, percentil`),filter(datos_paises,Date==max(datos_paises$Date)),col=cols) 
  
#  )
 

     
}    
 
 

