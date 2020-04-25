library(xlsxjars)
library(xlsx)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(lubridate)
library(googleVis)



avance_muertes<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",header=T,check.names=F)
avance_infectados<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",header=T,check.names=F)
desarrollo<-read.csv("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/0c406493-1104-4e2f-8265-443e3c37e2d9_Data.csv", header=T,check.names=F)
movimiento<-read.csv("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/applemobilitytrends-2020-04-17.csv", header=T,check.names=F)
gobernanza<-read.csv("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/WB\ Gobernanza/5aabee40-8a3c-4911-97d4-c0c9c8446188_Data.csv", header=T,check.names=F)

  #eci<-read.csv("eci_country_rankings_2013_2017.csv", header=T,check.names=F)



gobernanza<-mutate_all(gobernanza,as.character)
gobernanza$`2018 [YR2018]`<-as.numeric(gobernanza$`2018 [YR2018]`)
gobernanza[gobernanza==""]<-NA
gobernanza[gobernanza==".."]<-NA
gobernanza<-na.omit(gobernanza)
gobernanza$`Series Code`<-NULL
gobernanza<-spread(data=gobernanza,"Series Name","2018 [YR2018]")
gobernanza$`Control of Corruption: Estimate`<-NULL
gobernanza$`Government Effectiveness: Estimate`<-NULL
gobernanza$`Regulatory Quality: Estimate`<-NULL
gobernanza$`Voice and Accountability: Estimate`<-NULL
gobernanza$`Regulatory Quality, percentil`<-NULL
gobernanza$`Voice and Accountability, percentil`<-NULL


colnames(gobernanza)[which(names(gobernanza) == "Control of Corruption: Percentile Rank")] <- "Corruption control, percentil"
colnames(gobernanza)[which(names(gobernanza) == "Government Effectiveness: Percentile Rank")] <- "Government Effectiveness, percentil"
colnames(gobernanza)[which(names(gobernanza) == "Regulatory Quality: Percentile Rank")] <- "Regulatory Quality, percentil"
colnames(gobernanza)[which(names(gobernanza) == "Voice and Accountability: Percentile Rank")] <- "Voice and Accountability, percentil"


desarrollo<-mutate_all(desarrollo,as.character)
desarrollo$`2018 [YR2018]`<-as.numeric(desarrollo$`2018 [YR2018]`)
desarrollo[desarrollo==""]<-NA
desarrollo[desarrollo==".."]<-NA
desarrollo<-na.omit(desarrollo)
desarrollo$"2017 [YR2017]"<-NULL
desarrollo$"2016 [YR2016]"<-NULL
desarrollo$`Series Code`<-NULL
desarrollo<-spread(data=desarrollo,"Series Name","2018 [YR2018]")
desarrollo$`Number of deaths ages 5-14 years`<-NULL
desarrollo$`Number of infant deaths`<-NULL
desarrollo$`Physicians (per 1,000 people)`<-NULL
desarrollo$`Proportion of population spending more than 10% of household consumption or income on out-of-pocket health care expenditure (%)`<-NULL
desarrollo$`Proportion of population spending more than 25% of household consumption or income on out-of-pocket health care expenditure (%)`<-NULL
desarrollo$`Time to obtain an electrical connection (days)`<-NULL
desarrollo$`Population ages 65 and above (%)`<-desarrollo$`Population ages 65 and above, female (% of female population)`+desarrollo$`Population ages 65 and above, male (% of male population)`
desarrollo$`Population ages 65 and above, female (% of female population)`<-NULL
desarrollo$`Population ages 65 and above, male (% of male population)`<-NULL
desarrollo$`Vulnerable employment, total (% of total employment) (modeled ILO estimate)`<-NULL
desarrollo$`Real interest rate (%)`<-NULL
desarrollo$`Time to start a business`<-NULL
desarrollo$`Tuberculosis case detection rate (%, all forms)`<-NULL
desarrollo$`Surface area (sq. km)`<-NULL
desarrollo$`Time required to start a business (days)`<-NULL


colnames(desarrollo)[which(names(desarrollo) == "Control of Corruption: Estimate")] <- "Corruption control"
colnames(desarrollo)[which(names(desarrollo) == "Political Stability and Absence of Violence/Terrorism: Estimate")] <- "Political Stability"
colnames(desarrollo)[which(names(desarrollo) == "GDP per capita, PPP (current international $)")] <- "GDP per capita [USD]"
colnames(desarrollo)[which(names(desarrollo) == "Population, total")] <- "Population"
colnames(desarrollo)[which(names(desarrollo) == "Urban population (% of total population)")] <- "Urban population [%]"
colnames(desarrollo)[which(names(desarrollo) == "Population density (people per sq. km of land area)")] <- "Population density"
colnames(desarrollo)[which(names(desarrollo) == "Scientific and technical journal articles")] <- "Science and Tech articles"
colnames(desarrollo)[which(names(desarrollo) == "Population ages 65 and above (%)")] <- "Population above 65 [%]"


ultima_muertes<-colnames(avance_muertes)[ncol(avance_muertes)]
avance_muertes_3<-gather(avance_muertes,key="Date",value="Deaths","1/22/20":ultima_muertes)
avance_muertes_3<-aggregate(data=avance_muertes_3,`Deaths`~`Country/Region`+`Date`,sum)#####fecha
avance_muertes_3$Date<-parse_date_time(avance_muertes_3$Date, 'mdy')
avance_muertes_3$Date<-as.Date(avance_muertes_3$Date)

ultima_infectados<-colnames(avance_muertes)[ncol(avance_muertes)]
avance_infectados_3<-gather(avance_infectados,key="Date",value="Infected","1/22/20":ultima_infectados)
avance_infectados_3<-aggregate(data=avance_infectados_3,`Infected`~`Country/Region`+`Date`,sum)#####fecha
avance_infectados_3$Date<-parse_date_time(avance_infectados_3$Date, 'mdy')
avance_infectados_3$Date<-as.Date(avance_infectados_3$Date)

avance<-left_join(avance_infectados_3,avance_muertes_3)
rm(avance_infectados_3,avance_muertes_3)
colnames(avance)[which(names(avance) == "Country/Region")] <- "Country Name"
avance$`Country Name`<-gsub("US","United States",avance$`Country Name`)
avance$`Country Name`<-gsub("Egypt","Egypt, Arab Rep.",avance$`Country Name`)
avance$`Country Name`<-gsub("Venezuela","Venezuela, RB",avance$`Country Name`)
avance$`Country Name`<-gsub("Russia","Russian Federation",avance$`Country Name`)
avance$`Country Name`<-gsub("Korea, South","Korea, Rep.",avance$`Country Name`)
avance$`Country Name`<-gsub("Iran","Iran, Islamic Rep.",avance$`Country Name`)
avance$`Country Name`<-gsub("Kyrgyzstan","Kyrgyz Republic",avance$`Country Name`)
avance$`Country Name`<-gsub("Czechia","Czech Republic",avance$`Country Name`)
avance$`Country Name`<-gsub("Slovakia","Slovak Republic	",avance$`Country Name`)
avance$`Country Name`<-gsub("Syria","Syrian Arab Republic",avance$`Country Name`)
avance$`Country Name`<-gsub("Brunei","Brunei Darussalam",avance$`Country Name`)
avance$`Country Name`<-gsub("Congo (Brazzaville)","Congo, Rep.",avance$`Country Name`,fixed = TRUE)
avance$`Country Name`<-gsub("Congo (Kinshasa)","Congo, Dem. Rep.",avance$`Country Name`,fixed = TRUE)
avance$`Country Name`<-gsub("Saint Kitts and Nevis","St. Kitts and Nevis",avance$`Country Name`,fixed = TRUE)
avance$`Country Name`<-gsub("Saint Vincent and the Grenadines","St. Vincent and the Grenadines",avance$`Country Name`,fixed = TRUE)


#eci$`ECI+`<-NULL
#eci$`Country ID`<-NULL
#eci<-filter(eci,Year==2017)
#eci$Year<-NULL
#colnames(eci)[which(names(eci) == "ECI")] <- "ECI - 2017"
#colnames(eci)[which(names(eci) == "Country")] <- "Country Name"
#eci$`Country Name`<-gsub("Egypt","Egypt, Arab Rep.",eci$`Country Name`)




avance_2<-left_join(avance,desarrollo)
avance_2$"Deaths per 100000 inhabitants"<-(avance_2$Deaths/avance_2$Population)*100000
avance_2<-na.omit(avance_2)


#ultima_movimiento<-colnames(movimiento)[ncol(movimiento)]

#movimiento<-gather(movimiento,key="Date",value="Solicitudes","2020-01-13":ultima_movimiento)
#movimiento<-filter(movimiento,geo_type=="country/region")
#movimiento$geo_type<-as.character(movimiento$geo_type)
#movimiento$region<-as.character(movimiento$region)
#movimiento$transportation_type<-as.character(movimiento$transportation_type)
#movimiento$`Country Name`<-movimiento$`region`
#movimiento$region<-NULL
#movimiento$geo_type<-NULL
#movimiento<-spread(movimiento,transportation_type,Solicitudes)
#movimiento$transit<-NULL
#movimiento$Date<-as.Date(movimiento$Date)


avance_2<-left_join(avance_2,gobernanza)
avance_2<-na.omit(avance_2)
avance_2$`Country Code`<-NULL


#avance_3<-select(avance_2,"Deaths per 100000 inhabitants","GDP per capita [USD]","Population above 65 [%]","Population density","Population","Urban population","Urban population [%]","Deaths","driving","walking","Infected")



write.csv(avance_2,"www/datos_paises.csv",row.names = FALSE)


rm(avance,avance_infectados,avance_muertes,desarrollo)


avance_2_hoy<-filter(avance_2,avance_2$Date==as.Date("2020-04-21"))
avance_2_hoy<-filter(avance_2_hoy,`Deaths per 100000 inhabitants`>0)
avance_2_hoy<-filter(avance_2_hoy,avance_2_hoy$`Government Effectiveness, percentil`>65)
chile<-filter(avance_2_hoy,`Country Name`=="Chile")
g<-ggplot(avance_2_hoy,aes(`Government Effectiveness, percentil`,`Deaths per 100000 inhabitants`, label = `Country Name`))+geom_point()
g<-g+geom_text(size = 2.5,vjust = 0.1, nudge_y = 0.05,angle=10)
g<-g+scale_y_log10()
g


+geom_point(data=chile,colour="blue",size=1.4)+