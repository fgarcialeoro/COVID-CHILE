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



server <- function(session,input, output) {
  #source("grafico_burbujas_internacional.R")
 
url <- a("www.commonsense.cl", href="https://www.commonsense.cl")
output$tab <- renderUI({url})
 output$tab1 <- renderUI({url})
  ######################################################3
  
  
  
   ##gráfico de muertes vs otras variables

datos_paises<-read.csv("www/datos_paises.csv",check.names = FALSE)
datos_paises$Date<-as.Date(as.character(datos_paises$Date)) 
 
output$distPlot <- renderGvis(gvisMotionChart(datos_paises,idvar="Country Name",timevar="Date"))

reducida<- select(datos_paises,Date,`Deaths per 100000 inhabitants`,`GDP per capita [USD]`,`Population above 65 [%]`,`Science and Tech articles`,`Government Effectiveness, percentil`,`Voice and Accountability, percentil`)
reducida<-filter(reducida,Date==max(reducida$Date))
cols <-character(nrow(reducida))
cols[] <-"black"
cols[datos_paises$`Science and Tech articles`==	7121.74] <-"red"
cols[datos_paises$`GDP per capita [USD]`==25222.53] <-"red"
cols[datos_paises$`Population above 65 [%]`==	23.01721] <-"red"
cols[datos_paises$`Government Effectiveness, percentil`==81.73077]<-"red"

output$matriz <- renderPlot(
  
  pairs( ~log(`Deaths per 100000 inhabitants`)+log(`Science and Tech articles`)+log(`GDP per capita [USD]`)+`Population above 65 [%]`+log(`Government Effectiveness, percentil`),filter(datos_paises,Date==max(datos_paises$Date)),col=cols) 

)
 
   
   ###gráfico de contagiados por comuna
    output$espacio<-renderText("")
    output$titulo_mapas<-renderText("Mapas de contagio")
    
    
     output$comunas<-renderPlotly({
      
      avance_contagiados_chile<-read.csv("www/avance_contagiados_chile.csv",check.names = FALSE,)
      
      avance_contagiados_t<-avance_contagiados_chile
      avance_contagiados_t$Fecha<-as.Date(as.character(avance_contagiados_t$Fecha))
      avance_contagiados_t$REGION<-as.character(avance_contagiados_t$REGION)
      avance_contagiados_t$COMUNA<-as.character(avance_contagiados_t$COMUNA)
      avance_contagiados_t$Población<-as.numeric(as.character(avance_contagiados_t$Población))
      avance_contagiados_t$Confirmados<-as.numeric(as.character(avance_contagiados_t$Confirmados))
      avance_contagiados_t$Tasa.incidencia<-as.numeric(as.character(avance_contagiados_t$Tasa.incidencia))
      avance_contagiados_t$`Contagiados cada 100000 habitantes`<-as.numeric(as.character(avance_contagiados_t$`Contagiados cada 100000 habitantes`))
      avance_contagiados_t$"REGION-COMUNA"<-paste(avance_contagiados_t$REGION,avance_contagiados_t$COMUNA,sep="-")  
      
      if(input$i_ESCALAY =="Lineal"){escala<-scale_y_continuous()} else {escala<-scale_y_log10()}
      
      m<-ggplot(filter(avance_contagiados_t,`REGION-COMUNA` %in% input$i_REGION_COMUNA) , aes(Fecha,`Contagiados cada 100000 habitantes`,color = `REGION-COMUNA`))+geom_line(linetype=3,size = 0.1)+geom_point()
      m<-m+ggtitle("Valor acumulado de casos confirmados")+escala
      m<-ggplotly(m)
      m
            }) 
     
      
    output$crecimiento<-renderDataTable({
       arrange(read.csv("www/tasa_crecimiento.csv",check.names = FALSE), desc(`Tasa de crecimiento / %`))})
     
    
   
  output$mapacomunas<-renderLeaflet({
  

  s<-readRDS(paste("www/mapas/mapa_chile",toupper(input$i_REGION),input$i_FECHA,sep="-"))
  
  bins <- c(0,1,10,20,40,80,160,320,640,1200, Inf)
  pal <- colorBin("YlOrRd", domain = s$`Contagiados cada 100000 habitantes`, bins = bins)   
  leaflet(s)%>%
  addPolygons(fillColor = ~pal(`Contagiados cada 100000 habitantes`),weight =5,opacity = 1,color = "black",dashArray = "3",fillOpacity = 0.7,label=~COMUNA)%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLegend(pal = pal, values = ~`Contagiados cada 100000 habitantes`, opacity = 0.7, title = NULL,position = "bottomright")
     })   
}    
 
 

