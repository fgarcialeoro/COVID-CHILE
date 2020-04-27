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
gobernanza<-read.csv("/Users/franciscogarcia/Dropbox/working\ directory\ R/COVID\ CHILE/datos/WB\ Gobernanza/5aabee40-8a3c-4911-97d4-c0c9c8446188_Data.csv", header=T,check.names=F)


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



avance_2<-left_join(avance,desarrollo)
avance_2$"Deaths per 100000 inhabitants"<-(avance_2$Deaths/avance_2$Population)*100000
avance_2<-na.omit(avance_2)


avance_2<-left_join(avance_2,gobernanza)
avance_2<-na.omit(avance_2)
avance_2$`Country Code`<-NULL



write.csv(avance_2,"www/datos_paises.csv",row.names = FALSE)
