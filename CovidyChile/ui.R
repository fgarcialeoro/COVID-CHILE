library(plotly)
library(xlsxjars)
library(xlsx)
library(dplyr)
library(tidyr)
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
      tabPanel("Casos a nivel nacional",
               sidebarLayout(
                 sidebarPanel(
                   div("Los datos son obtenidos del repositorio", a("GitHub", href="https://github.com/MinCiencia/Datos-COVID19"),
                       "del Ministerio de Ciencia y Tecnología"),br(),br(),br(),br(),
                   img(src ="logocsc.png", align = "center"),
                   uiOutput("tab3")
                 ),
                 mainPanel(
                   plotlyOutput("todos"),plotlyOutput("todos_normalizado")
                   )
               )),
      
      tabPanel("Tasa de crecimiento nacional",
               sidebarLayout(
                 sidebarPanel(
                   div("Los datos son obtenidos del repositorio", a("GitHub", href="https://github.com/MinCiencia/Datos-COVID19"),
                       "del Ministerio de Ciencia y Tecnología"),br(),br(),br(),br(),
                   img(src ="logocsc.png", align = "center"),
                   uiOutput("tab4")
                 ),
                 mainPanel(
                   plotlyOutput("var_tasa")
                 )
               )),
      
      tabPanel("Uso de camas",
               sidebarLayout(
                 sidebarPanel(
                   div("Los datos son obtenidos del repositorio", a("GitHub", href="https://github.com/MinCiencia/Datos-COVID19"),
                       "del Ministerio de Ciencia y Tecnología"),br(),br(),br(),br(),
                   img(src ="logocsc.png", align = "center"),
                   uiOutput("tab5")
                 ),
                 mainPanel(
                   plotlyOutput("camas_en_uso")
                 )
               )),
      
        tabPanel("Casos actuales y comunas", 
                 sidebarLayout(
                   sidebarPanel(
                     pickerInput("i_REGION_COMUNA", "Región y Comuna",
                                 choices=c("Arica y Parinacota-Arica",	"Arica y Parinacota-Camarones",	"Arica y Parinacota-General Lagos",	"Arica y Parinacota-Putre",	"Tarapacá-Alto Hospicio",	"Tarapacá-Camina",	"Tarapacá-Colchane",	"Tarapacá-Huara",	"Tarapacá-Iquique",	"Tarapacá-Pica",	"Tarapacá-Pozo Almonte",	"Antofagasta-Antofagasta",	"Antofagasta-Calama",	"Antofagasta-Maria Elena",	"Antofagasta-Mejillones",	"Antofagasta-Ollague",	"Antofagasta-San Pedro de Atacama",	"Antofagasta-Sierra Gorda",	"Antofagasta-Taltal",	"Antofagasta-Tocopilla",	"Atacama-Alto del Carmen",	"Atacama-Caldera",	"Atacama-Chanaral",	"Atacama-Copiapo",	"Atacama-Diego de Almagro",	"Atacama-Freirina",	"Atacama-Huasco",	"Atacama-Tierra Amarilla",	"Atacama-Vallenar",	"Coquimbo-Andacollo",	"Coquimbo-Canela",	"Coquimbo-Combarbala",	"Coquimbo-Coquimbo",	"Coquimbo-Illapel",	"Coquimbo-La Higuera",	"Coquimbo-La Serena",	"Coquimbo-Los Vilos",	"Coquimbo-Monte Patria",	"Coquimbo-Ovalle",	"Coquimbo-Paiguano",	"Coquimbo-Punitaqui",	"Coquimbo-Rio Hurtado",	"Coquimbo-Salamanca",	"Coquimbo-Vicuna",	"Valparaíso-Algarrobo",	"Valparaíso-Cabildo",	"Valparaíso-Calera",	"Valparaíso-Calle Larga",	"Valparaíso-Cartagena",	"Valparaíso-Casablanca",	"Valparaíso-Catemu",	"Valparaíso-Concon",	"Valparaíso-El Quisco",	"Valparaíso-El Tabo",	"Valparaíso-Hijuelas",	"Valparaíso-Isla de Pascua",	"Valparaíso-Juan Fernandez",	"Valparaíso-La Cruz",	"Valparaíso-La Ligua",	"Valparaíso-Limache",	"Valparaíso-Llaillay",	"Valparaíso-Los Andes",	"Valparaíso-Nogales",	"Valparaíso-Olmue",	"Valparaíso-Panquehue",	"Valparaíso-Papudo",	"Valparaíso-Petorca",	"Valparaíso-Puchuncavi",	"Valparaíso-Putaendo",	"Valparaíso-Quillota",	"Valparaíso-Quilpue",	"Valparaíso-Quintero",	"Valparaíso-Rinconada",	"Valparaíso-San Antonio",	"Valparaíso-San Esteban",	"Valparaíso-San Felipe",	"Valparaíso-Santa Maria",	"Valparaíso-Santo Domingo",	"Valparaíso-Valparaiso",	"Valparaíso-Villa Alemana",	"Valparaíso-Vina del Mar",	"Valparaíso-Zapallar",	"Metropolitana-Alhue",	"Metropolitana-Buin",	"Metropolitana-Calera de Tango",	"Metropolitana-Cerrillos",	"Metropolitana-Cerro Navia",	"Metropolitana-Colina",	"Metropolitana-Conchali",	"Metropolitana-Curacavi",	"Metropolitana-El Bosque",	"Metropolitana-El Monte",	"Metropolitana-Estacion Central",	"Metropolitana-Huechuraba",	"Metropolitana-Independencia",	"Metropolitana-Isla de Maipo",	"Metropolitana-La Cisterna",	"Metropolitana-La Florida",	"Metropolitana-La Granja",	"Metropolitana-La Pintana",	"Metropolitana-La Reina",	"Metropolitana-Lampa",	"Metropolitana-Las Condes",	"Metropolitana-Lo Barnechea",	"Metropolitana-Lo Espejo",	"Metropolitana-Lo Prado",	"Metropolitana-Macul",	"Metropolitana-Maipu",	"Metropolitana-Maria Pinto",	"Metropolitana-Melipilla",	"Metropolitana-Nunoa",	"Metropolitana-Padre Hurtado",	"Metropolitana-Paine",	"Metropolitana-Pedro Aguirre Cerda",	"Metropolitana-Penaflor",	"Metropolitana-Penalolen",	"Metropolitana-Pirque",	"Metropolitana-Providencia",	"Metropolitana-Pudahuel",	"Metropolitana-Puente Alto",	"Metropolitana-Quilicura",	"Metropolitana-Quinta Normal",	"Metropolitana-Recoleta",	"Metropolitana-Renca",	"Metropolitana-San Bernardo",	"Metropolitana-San Joaquin",	"Metropolitana-San Jose de Maipo",	"Metropolitana-San Miguel",	"Metropolitana-San Pedro",	"Metropolitana-San Ramon",	"Metropolitana-Santiago",	"Metropolitana-Talagante",	"Metropolitana-Tiltil",	"Metropolitana-Vitacura",	"O’Higgins-Chepica",	"O’Higgins-Chimbarongo",	"O’Higgins-Codegua",	"O’Higgins-Coinco",	"O’Higgins-Coltauco",	"O’Higgins-Donihue",	"O’Higgins-Graneros",	"O’Higgins-La Estrella",	"O’Higgins-Las Cabras",	"O’Higgins-Litueche",	"O’Higgins-Lolol",	"O’Higgins-Machali",	"O’Higgins-Malloa",	"O’Higgins-Marchihue",	"O’Higgins-Mostazal",	"O’Higgins-Nancagua",	"O’Higgins-Navidad",	"O’Higgins-Olivar",	"O’Higgins-Palmilla",	"O’Higgins-Paredones",	"O’Higgins-Peralillo",	"O’Higgins-Peumo",	"O’Higgins-Pichidegua",	"O’Higgins-Pichilemu",	"O’Higgins-Placilla",	"O’Higgins-Pumanque",	"O’Higgins-Quinta de Tilcoco",	"O’Higgins-Rancagua",	"O’Higgins-Rengo",	"O’Higgins-Requinoa",	"O’Higgins-San Fernando",	"O’Higgins-San Vicente",	"O’Higgins-Santa Cruz",	"Maule-Cauquenes",	"Maule-Chanco",	"Maule-Colbun",	"Maule-Constitucion",	"Maule-Curepto",	"Maule-Curico",	"Maule-Empedrado",	"Maule-Hualane",	"Maule-Licanten",	"Maule-Linares",	"Maule-Longavi",	"Maule-Maule",	"Maule-Molina",	"Maule-Parral",	"Maule-Pelarco",	"Maule-Pelluhue",	"Maule-Pencahue",	"Maule-Rauco",	"Maule-Retiro",	"Maule-Rio Claro",	"Maule-Romeral",	"Maule-Sagrada Familia",	"Maule-San Clemente",	"Maule-San Javier",	"Maule-San Rafael",	"Maule-Talca",	"Maule-Teno",	"Maule-Vichuquen",	"Maule-Villa Alegre",	"Maule-Yerbas Buenas",	"Ñuble-Bulnes",	"Ñuble-Chillan",	"Ñuble-Chillan Viejo",	"Ñuble-Cobquecura",	"Ñuble-Coelemu",	"Ñuble-Coihueco",	"Ñuble-El Carmen",	"Ñuble-Ninhue",	"Ñuble-Niquen",	"Ñuble-Pemuco",	"Ñuble-Pinto",	"Ñuble-Portezuelo",	"Ñuble-Quillon",	"Ñuble-Quirihue",	"Ñuble-Ranquil",	"Ñuble-San Carlos",	"Ñuble-San Fabian",	"Ñuble-San Ignacio",	"Ñuble-San Nicolas",	"Ñuble-Treguaco",	"Ñuble-Yungay",	"Biobío-Alto Biobio",	"Biobío-Antuco",	"Biobío-Arauco",	"Biobío-Cabrero",	"Biobío-Canete",	"Biobío-Chiguayante",	"Biobío-Concepcion",	"Biobío-Contulmo",	"Biobío-Coronel",	"Biobío-Curanilahue",	"Biobío-Florida",	"Biobío-Hualpen",	"Biobío-Hualqui",	"Biobío-Laja",	"Biobío-Lebu",	"Biobío-Los Alamos",	"Biobío-Los Angeles",	"Biobío-Lota",	"Biobío-Mulchen",	"Biobío-Nacimiento",	"Biobío-Negrete",	"Biobío-Penco",	"Biobío-Quilaco",	"Biobío-Quilleco",	"Biobío-San Pedro de la Paz",	"Biobío-San Rosendo",	"Biobío-Santa Barbara",	"Biobío-Santa Juana",	"Biobío-Talcahuano",	"Biobío-Tirua",	"Biobío-Tome",	"Biobío-Tucapel",	"Biobío-Yumbel",	"Araucanía-Angol",	"Araucanía-Carahue",	"Araucanía-Cholchol",	"Araucanía-Collipulli",	"Araucanía-Cunco",	"Araucanía-Curacautin",	"Araucanía-Curarrehue",	"Araucanía-Ercilla",	"Araucanía-Freire",	"Araucanía-Galvarino",	"Araucanía-Gorbea",	"Araucanía-Lautaro",	"Araucanía-Loncoche",	"Araucanía-Lonquimay",	"Araucanía-Los Sauces",	"Araucanía-Lumaco",	"Araucanía-Melipeuco",	"Araucanía-Nueva Imperial",	"Araucanía-Padre Las Casas",	"Araucanía-Perquenco",	"Araucanía-Pitrufquen",	"Araucanía-Pucon",	"Araucanía-Puren",	"Araucanía-Renaico",	"Araucanía-Saavedra",	"Araucanía-Temuco",	"Araucanía-Teodoro Schmidt",	"Araucanía-Tolten",	"Araucanía-Traiguen",	"Araucanía-Victoria",	"Araucanía-Vilcun",	"Araucanía-Villarrica",	"Los Ríos-Corral",	"Los Ríos-Futrono",	"Los Ríos-La Union",	"Los Ríos-Lago Ranco",	"Los Ríos-Lanco",	"Los Ríos-Los Lagos",	"Los Ríos-Mafil",	"Los Ríos-Mariquina",	"Los Ríos-Paillaco",	"Los Ríos-Panguipulli",	"Los Ríos-Rio Bueno",	"Los Ríos-Valdivia",	"Los Lagos-Ancud",	"Los Lagos-Calbuco",	"Los Lagos-Castro",	"Los Lagos-Chaiten",	"Los Lagos-Chonchi",	"Los Lagos-Cochamo",	"Los Lagos-Curaco de Velez",	"Los Lagos-Dalcahue",	"Los Lagos-Fresia",	"Los Lagos-Frutillar",	"Los Lagos-Futaleufu",	"Los Lagos-Hualaihue",	"Los Lagos-Llanquihue",	"Los Lagos-Los Muermos",	"Los Lagos-Maullin",	"Los Lagos-Osorno",	"Los Lagos-Palena",	"Los Lagos-Puerto Montt",	"Los Lagos-Puerto Octay",	"Los Lagos-Puerto Varas",	"Los Lagos-Puqueldon",	"Los Lagos-Purranque",	"Los Lagos-Puyehue",	"Los Lagos-Queilen",	"Los Lagos-Quellon",	"Los Lagos-Quemchi",	"Los Lagos-Quinchao",	"Los Lagos-Rio Negro",	"Los Lagos-San Juan de la Costa",	"Los Lagos-San Pablo",	"Aysén-Aysen",	"Aysén-Chile Chico",	"Aysén-Cisnes",	"Aysén-Cochrane",	"Aysén-Coyhaique",	"Aysén-Guaitecas",	"Aysén-Lago Verde",	"Aysén-OHiggins",	"Aysén-Rio Ibanez",	"Aysén-Tortel",	"Magallanes-Antartica",	"Magallanes-Cabo de Hornos",	"Magallanes-Laguna Blanca",	"Magallanes-Natales",	"Magallanes-Porvenir",	"Magallanes-Primavera",	"Magallanes-Punta Arenas",	"Magallanes-Rio Verde",	"Magallanes-San Gregorio",	"Magallanes-Timaukel",	"Magallanes-Torres del Paine"),
                                 multiple = TRUE,selected=c("Metropolitana-Providencia"),
                                 options = pickerOptions(actionsBox = TRUE,liveSearch = TRUE)),
                     radioButtons("i_ESCALAY","Escala del eje vertical",choices=c("Lineal","Logarítmica")),br(),br(),br(),br(),br(),br(),br(),br(),
                     div("Los datos son obtenidos del repositorio", a("GitHub", href="https://github.com/MinCiencia/Datos-COVID19"),
                         "del Ministerio de Ciencia y Tecnología"),br(),br(),br(),br(),
                     
                     img(src ="logocsc.png", align = "center"),
                     uiOutput("tab")
                     ),
                   
                   
                   mainPanel(verticalLayout(
                     plotlyOutput("comunas_cada_100_mil"),br(),plotlyOutput("comunas_absoluto"),br(),br(),
                     div(strong("Estimación de la tasa de crecimiento de casos confirmados por comuna"),br(),"En la tabla a continuación se entrega una estimación 
                  de la tasa de crecimiento del número de casos confirmados acumulados.",
                     dataTableOutput("crecimiento"))))
                   ))
        
      #tabPanel("Mundo: COVID y otros indicadores",
      #      sidebarLayout(
      #  sidebarPanel(
      #    div("Los datos son obtenidos del repositorio", a("GitHub", href="https://github.com/CSSEGISandData/COVID-19"),
      #        "de CSSE at Johns Hopkins University", "y del",a("WB Data Bank.", href="https://databank.worldbank.org/home.aspx"),"Estos últimos son los correspondientes al año 2018"),br(),br(),br(),br(),
      #    img(src ="logocsc.png", align = "center"),
      #      uiOutput("tab4")
      #   ),
      ##   mainPanel(
      #     #div(strong("Variación del número de fallecidos a nivel internacional, en relación con variables económicas"),"Aun cuando los valores se estabilizarán en el mediano plazo, ya es posible apreciar algunas tendencias. El círculo rojo representa a Chile."),
      #     #plotOutput("matriz"),br(),br(),
      #    div(strong("Variación del número de fallecidos e infectados a nivel internacional, en relación con variables económicas"),"Aun cuando los valores se estabilizarán en el mediano plazo, ya es posible apreciar algunas tendencias.El usuario debe seleccionar las variables. La fuente de las variables económicas es el Banco Mundial y los valores corresponden al año 2018; están fijos."),
      #    htmlOutput("distPlot")
      #      
      #  )
      #))
        
        
        
             ))





