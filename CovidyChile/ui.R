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
library(shinyWidgets)
library(stringi)
library(DT)

Verde_oscuro<-rgb(73,79,14,maxColorValue=255)
Azul_claro<-rgb(0,71,130, maxColorValue=255)
Rojo<-rgb(143,19,14, maxColorValue=255)
Verde_claro<-rgb(198,206,6,maxColorValue=255)



ui <- fluidPage(  
    
  
    titlePanel("COVID-19 en Chile."),
    
    tabsetPanel( 
         
        tabPanel("Casos confirmados acumulados y comunas", 
                 sidebarLayout(
                   sidebarPanel(
                     pickerInput("i_REGION_COMUNA", "Región y Comuna",
                                 choices=c("Aysén del General Carlos Ibáñez del Campo-Aisén","Aysén del General Carlos Ibáñez del Campo-Chile Chico","Aysén del General Carlos Ibáñez del Campo-Cisnes","Aysén del General Carlos Ibáñez del Campo-Cochrane","Aysén del General Carlos Ibáñez del Campo-Coihaique","Aysén del General Carlos Ibáñez del Campo-Guaitecas","Aysén del General Carlos Ibáñez del Campo-Lago Verde","Aysén del General Carlos Ibáñez del Campo-O'Higgins","Aysén del General Carlos Ibáñez del Campo-Primavera","Aysén del General Carlos Ibáñez del Campo-Río Ibáñez","Aysén del General Carlos Ibáñez del Campo-Tortel","Antofagasta-Antofagasta","Antofagasta-Calama","Antofagasta-María Elena","Antofagasta-Mejillones","Antofagasta-Ollagüe","Antofagasta-San Pedro de Atacama","Antofagasta-Sierra Gorda","Antofagasta-Taltal","Antofagasta-Tocopilla","La Araucanía-Angol","La Araucanía-Carahue","La Araucanía-Cholchol","La Araucanía-Collipulli","La Araucanía-Cunco","La Araucanía-Curacautín","La Araucanía-Curarrehue","La Araucanía-Ercilla","La Araucanía-Freire","La Araucanía-Galvarino","La Araucanía-Gorbea","La Araucanía-Lautaro","La Araucanía-Loncoche","La Araucanía-Lonquimay","La Araucanía-Los Sauces","La Araucanía-Lumaco","La Araucanía-Melipeuco","La Araucanía-Nueva Imperial","La Araucanía-Padre Las Casas","La Araucanía-Perquenco","La Araucanía-Pitrufquén","La Araucanía-Pucón","La Araucanía-Purén","La Araucanía-Renaico","La Araucanía-Saavedra","La Araucanía-Temuco","La Araucanía-Teodoro Schmidt","La Araucanía-Toltén","La Araucanía-Traiguén","La Araucanía-Victoria","La Araucanía-Vilcún","La Araucanía-Villarrica","Arica y Parinacota-Arica","Arica y Parinacota-Camarones","Arica y Parinacota-General Lagos","Arica y Parinacota-Putre","Atacama-Alto del Carmen","Atacama-Caldera","Atacama-Chañaral","Atacama-Copiapó","Atacama-Diego de Almagro","Atacama-Freirina","Atacama-Huasco","Atacama-Tierra Amarilla","Atacama-Vallenar","Biobío-Alto Biobío","Biobío-Antuco","Biobío-Arauco","Biobío-Cabrero","Biobío-Cañete","Biobío-Chiguayante","Biobío-Concepción","Biobío-Contulmo","Biobío-Coronel","Biobío-Curanilahue","Biobío-Florida","Biobío-Hualpén","Biobío-Hualqui","Biobío-Laja","Biobío-Lebu","Biobío-Los Álamos","Biobío-Los Ángeles","Biobío-Lota","Biobío-Mulchén","Biobío-Nacimiento","Biobío-Negrete","Biobío-Penco","Biobío-Quilaco","Biobío-Quilleco","Biobío-San Pedro de la Paz","Biobío-San Rosendo","Biobío-Santa Bárbara","Biobío-Santa Juana","Biobío-Talcahuano","Biobío-Tirúa","Biobío-Tomé","Biobío-Tucapel","Biobío-Yumbel","Coquimbo-Andacollo","Coquimbo-Canela","Coquimbo-Combarbalá","Coquimbo-Coquimbo","Coquimbo-Illapel","Coquimbo-La Higuera","Coquimbo-La Serena","Coquimbo-Los Vilos","Coquimbo-Monte Patria","Coquimbo-Ovalle","Coquimbo-Paiguano","Coquimbo-Punitaqui","Coquimbo-Río Hurtado","Coquimbo-Salamanca","Coquimbo-Vicuña",
                                           "Libertador General Bernardo O'Higgins-Chépica","Libertador General Bernardo O'Higgins-Chimbarongo","Libertador General Bernardo O'Higgins-Codegua","Libertador General Bernardo O'Higgins-Coinco","Libertador General Bernardo O'Higgins-Coltauco","Libertador General Bernardo O'Higgins-Doñihue","Libertador General Bernardo O'Higgins-Graneros","Libertador General Bernardo O'Higgins-La Estrella","Libertador General Bernardo O'Higgins-Las Cabras","Libertador General Bernardo O'Higgins-Litueche","Libertador General Bernardo O'Higgins-Lolol","Libertador General Bernardo O'Higgins-Machalí","Libertador General Bernardo O'Higgins-Malloa","Libertador General Bernardo O'Higgins-Marchihue","Libertador General Bernardo O'Higgins-Mostazal","Libertador General Bernardo O'Higgins-Nancagua","Libertador General Bernardo O'Higgins-Navidad","Libertador General Bernardo O'Higgins-Olivar","Libertador General Bernardo O'Higgins-Palmilla","Libertador General Bernardo O'Higgins-Paredones","Libertador General Bernardo O'Higgins-Peralillo","Libertador General Bernardo O'Higgins-Peumo","Libertador General Bernardo O'Higgins-Pichidegua","Libertador General Bernardo O'Higgins-Pichilemu","Libertador General Bernardo O'Higgins-Placilla","Libertador General Bernardo O'Higgins-Pumanque","Libertador General Bernardo O'Higgins-Quinta de Tilcoco","Libertador General Bernardo O'Higgins-Rancagua","Libertador General Bernardo O'Higgins-Rengo","Libertador General Bernardo O'Higgins-Requínoa","Libertador General Bernardo O'Higgins-San Fernando","Libertador General Bernardo O'Higgins-San Vicente","Libertador General Bernardo O'Higgins-Santa Cruz","Los Lagos-Ancud","Los Lagos-Calbuco","Los Lagos-Castro","Los Lagos-Chaitén","Los Lagos-Chonchi","Los Lagos-Cochamó","Los Lagos-Curaco de Vélez","Los Lagos-Dalcahue","Los Lagos-Fresia","Los Lagos-Frutillar","Los Lagos-Futaleufú","Los Lagos-Hualaihué","Los Lagos-Llanquihue","Los Lagos-Los Muermos","Los Lagos-Maullín","Los Lagos-Osorno","Los Lagos-Palena","Los Lagos-Puerto Montt","Los Lagos-Puerto Octay","Los Lagos-Puerto Varas","Los Lagos-Puqueldón","Los Lagos-Purranque","Los Lagos-Puyehue","Los Lagos-Queilén","Los Lagos-Quellón","Los Lagos-Quemchi","Los Lagos-Quinchao","Los Lagos-Río Negro","Los Lagos-San Juan de la Costa","Los Lagos-San Pablo","Los Ríos-Corral","Los Ríos-Futrono","Los Ríos-La Unión","Los Ríos-Lago Ranco","Los Ríos-Lanco","Los Ríos-Los Lagos","Los Ríos-Máfil","Los Ríos-Mariquina","Los Ríos-Paillaco","Los Ríos-Panguipulli","Los Ríos-Río Bueno","Los Ríos-Valdivia","Magallanes y de la Antártica Chilena-Antártica","Magallanes y de la Antártica Chilena-Cabo de Hornos","Magallanes y de la Antártica Chilena-Laguna Blanca","Magallanes y de la Antártica Chilena-Natales","Magallanes y de la Antártica Chilena-Porvenir","Magallanes y de la Antártica Chilena-Primavera","Magallanes y de la Antártica Chilena-Punta Arenas","Magallanes y de la Antártica Chilena-Río Verde","Magallanes y de la Antártica Chilena-San Gregorio","Magallanes y de la Antártica Chilena-Timaukel","Magallanes y de la Antártica Chilena-Torres del Paine","Maule-Cauquenes","Maule-Chanco","Maule-Colbún","Maule-Constitución","Maule-Curepto","Maule-Curicó","Maule-Empedrado","Maule-Hualañé","Maule-Licantén","Maule-Linares","Maule-Longaví","Maule-Maule","Maule-Molina","Maule-Parral","Maule-Pelarco","Maule-Pelluhue","Maule-Pencahue","Maule-Rauco","Maule-Retiro","Maule-Río Claro","Maule-Romeral","Maule-Sagrada Familia","Maule-San Clemente","Maule-San Javier","Maule-San Rafael","Maule-Talca","Maule-Teno","Maule-Vichuquén","Maule-Villa Alegre","Maule-Yerbas Buenas","Metropolitana de Santiago-Alhué","Metropolitana de Santiago-Buin","Metropolitana de Santiago-Calera de Tango","Metropolitana de Santiago-Cerrillos","Metropolitana de Santiago-Cerro Navia","Metropolitana de Santiago-Colina","Metropolitana de Santiago-Conchalí","Metropolitana de Santiago-Curacaví","Metropolitana de Santiago-El Bosque","Metropolitana de Santiago-El Monte","Metropolitana de Santiago-Estación Central","Metropolitana de Santiago-Huechuraba","Metropolitana de Santiago-Independencia","Metropolitana de Santiago-Isla de Maipo","Metropolitana de Santiago-La Cisterna","Metropolitana de Santiago-La Florida","Metropolitana de Santiago-La Granja","Metropolitana de Santiago-La Pintana","Metropolitana de Santiago-La Reina","Metropolitana de Santiago-Lampa","Metropolitana de Santiago-Las Condes","Metropolitana de Santiago-Lo Barnechea","Metropolitana de Santiago-Lo Espejo","Metropolitana de Santiago-Lo Prado","Metropolitana de Santiago-Macul","Metropolitana de Santiago-Maipú","Metropolitana de Santiago-María Pinto","Metropolitana de Santiago-Melipilla","Metropolitana de Santiago-Ñuñoa","Metropolitana de Santiago-Padre Hurtado","Metropolitana de Santiago-Paine","Metropolitana de Santiago-Pedro Aguirre Cerda","Metropolitana de Santiago-Peñaflor","Metropolitana de Santiago-Peñalolén","Metropolitana de Santiago-Pirque","Metropolitana de Santiago-Providencia","Metropolitana de Santiago-Pudahuel","Metropolitana de Santiago-Puente Alto","Metropolitana de Santiago-Quilicura","Metropolitana de Santiago-Quinta Normal","Metropolitana de Santiago-Recoleta","Metropolitana de Santiago-Renca","Metropolitana de Santiago-San Bernardo","Metropolitana de Santiago-San Joaquín","Metropolitana de Santiago-San José de Maipo","Metropolitana de Santiago-San Miguel","Metropolitana de Santiago-San Pedro","Metropolitana de Santiago-San Ramón","Metropolitana de Santiago-Santiago","Metropolitana de Santiago-Talagante","Metropolitana de Santiago-Tiltil","Metropolitana de Santiago-Vitacura","Ñuble-Bulnes","Ñuble-Chillán Viejo","Ñuble-Chillán","Ñuble-Cobquecura","Ñuble-Coelemu","Ñuble-Coihueco","Ñuble-El Carmen","Ñuble-Ninhue","Ñuble-Ñiquén","Ñuble-Pemuco","Ñuble-Pinto","Ñuble-Portezuelo","Ñuble-Quillón","Ñuble-Quirihue","Ñuble-Ránquil","Ñuble-San Carlos","Ñuble-San Fabián","Ñuble-San Ignacio","Ñuble-San Nicolás","Ñuble-Treguaco","Ñuble-Yungay","Tarapacá-Alto Hospicio","Tarapacá-Camiña","Tarapacá-Colchane","Tarapacá-Huara","Tarapacá-Iquique","Tarapacá-Pica","Tarapacá-Pozo Almonte","Valparaíso-Algarrobo","Valparaíso-Cabildo","Valparaíso-Calera","Valparaíso-Calle Larga","Valparaíso-Cartagena","Valparaíso-Casablanca","Valparaíso-Catemu","Valparaíso-Concón","Valparaíso-El Quisco","Valparaíso-El Tabo","Valparaíso-Hijuelas","Valparaíso-Isla de Pascua","Valparaíso-Juan Fernández","Valparaíso-La Cruz","Valparaíso-La Ligua","Valparaíso-Limache","Valparaíso-Llaillay","Valparaíso-Los Andes","Valparaíso-Nogales","Valparaíso-Olmué","Valparaíso-Panquehue","Valparaíso-Papudo","Valparaíso-Petorca","Valparaíso-Puchuncaví","Valparaíso-Putaendo","Valparaíso-Quillota","Valparaíso-Quilpué","Valparaíso-Quintero","Valparaíso-Rinconada","Valparaíso-San Antonio","Valparaíso-San Esteban","Valparaíso-San Felipe","Valparaíso-Santa María","Valparaíso-Santo Domingo","Valparaíso-Valparaíso","Valparaíso-Villa Alemana","Valparaíso-Viña del Mar"
                                                                                ,"Valparaíso-Zapallar"),
                                 multiple = TRUE,selected=c("Metropolitana de Santiago-Providencia"),
                                 options = pickerOptions(actionsBox = TRUE,liveSearch = TRUE)),
                     radioButtons("i_ESCALAY","Escala del eje vertical",choices=c("Lineal","Logarítmica")),
                     img(src ="logocsc.png", align = "center"),
                     uiOutput("tab")
                     ),
                   
                   
                   mainPanel(verticalLayout(
                     plotlyOutput("comunas"),
                     div(strong("Estimación de la tasa de crecimiento de casos confirmados por comuna"),br(),"En la tabla a continuación se entrega una estimación 
                  de la tasa de crecimiento del número de casos confirmados acumulados.  Esta tasa se evaluó a partir de un modelo exponencial aplicado por separado a cada Comuna, considerando que estamos en la etapa inicial de la propagación de los contagios. Para algunas comunas no se pudo ajustar el modelo.  Se incluye la incertidumbre expandida de la tasa de crecimiento, para un intervalode covertura de un 95%, esta está asociada a los errores aleatorios del modelo.  En muchas situaciones la incertidumbre
                      no se considera y se da por seguro el valor predicho por el modelo, (en este caso la tasa), cayendo en la trampa del promedio.  Esta trampa del promedio genera un riesgo de decisión,
                      que es un sesgo de decisión. La incertidumbre puede ser menor con otro modelo, pero siempre va a estar presente. Dado que a nivel de Comuna, son pocos los datos, las incertidumbres son grandes. Por otra parte, no se ha considerado la incertidumbre del muestreo, 
                      ni las características de sensibilidad ni precisión de los exámenes. Una tasa de crecimiento de 20% implica un incremento de 20% de casos por día",
                     dataTableOutput("crecimiento"))))
                   )),
        
        
        tabPanel("Casos confirmados acumulados y mapas",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("i_REGION", "Región", choices=toupper(c("Arica y Parinacota","Tarapaca","Antofagasta","Atacama",
                                            "Coquimbo","Valparaiso","Metropolitana de Santiago",
                                             "Libertador General Bernardo O'Higgins","Maule","Nuble",
                                                "Biobio","La Araucania","Los Rios","Los Lagos",
                                                  "Aysen del General Carlos Ibanez del Campo",
                                                   "Magallanes y de la Antartica Chilena")),selected=toupper(c("Metropolitana de Santiago"))),
                     #sliderInput("i_FECHA","Fecha",min=as.Date("2020-03-30"),max=as.Date("2020-04-20"),value=as.Date("2020-04-20"), ticks=FALSE),
                     selectInput("i_FECHA","Fecha",choices=c("2020-03-30","2020-04-01","2020-04-03","2020-04-06","2020-04-08","2020-04-10","2020-04-15","2020-04-17","2020-04-20"),selected="2020-04-20"),
                     img(src ="logocsc.png", align = "center"),
                     uiOutput("tab1")
                     ),
                   
                   mainPanel(
                     div(strong("Número de casos confirmados en comunas cada 100000 habitantes")),
                     leafletOutput("mapacomunas"))
                   )
                 ),
        
        
        tabPanel("Resumen nacional",
                 sidebarLayout(
                   sidebarPanel(
                     img(src ="logocsc.png", align = "center"),
                     uiOutput("tab3")
                   ),
                 mainPanel(
                   plotlyOutput("todos"),br(),br(),
                   plotlyOutput("var_tasa"),div("En este gráfico se presenta la tasa de crecimiento evaluada para cada día, utilizando un modelo exponencial.  Hasta el día 10 se consideraron todos los días previos, luego se tomaron en cuenta intervalos de cinco días"),br(),br()
                                 )
        )),
        
        tabPanel("Mundo: COVID y otros indicadores",
                 sidebarLayout(
                   sidebarPanel(
                     img(src ="logocsc.png", align = "center"),
                     uiOutput("tab4")
                   ),
                 mainPanel(
                   div(strong("Variación del número de fallecidos a nivel internacional, en relación con variables económicas"),"Aun cuando los valores se estabilizarán en el mediano plazo, ya es posible apreciar algunas tendencias. El círculo rojo representa a Chile."),
                   plotOutput("matriz"),br(),br(),
                   div(strong("Variación del número de fallecidos e infectados a nivel internacional, en relación con variables económicas y de movilidad de apple"),"Aun cuando los valores se estabilizarán en el mediano plazo, ya es posible apreciar algunas tendencias.El usuario debe seleccionar las variables. La fuente de las variables económicas es el Banco Mundial y los valores corresponden al año 2018; están fijos."),
                   htmlOutput("distPlot")
                   
                   )
                 ))
       
)
)




