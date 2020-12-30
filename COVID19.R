library(tidyverse)
COVID19<-read.csv("covid_19_data.csv")
COVID19<-as.tibble(COVID19)
COVID19
COVID19$ObservationDate<-as.Date(COVID19$ObservationDate,  format = "%m/%d/%Y" )
China<-COVID19 %>%
  filter(Country.Region=="Mainland China")
China
China$ObservationDate<-as.Date(China$ObservationDate, format = "%m/%d/%Y" )
China
#China cases
library(dplyr)
ggplot(data=China)+
  geom_point(mapping=aes(x=ObservationDate, y=Confirmed, color=Province.State))+
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(size = 14))

ggplot(China, aes(x=as.numeric(ObservationDate), y=Confirmed, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Confirmed")
ggplot(China, aes(x=ObservationDate, y=Confirmed, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Confirmed")


ggplot(China, aes(x=as.numeric(ObservationDate), y=Deaths, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Deaths")
ggplot(China, aes(x=ObservationDate, y=Deaths, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Deaths")

ggplot(China, aes(x=as.numeric(ObservationDate), y=Recovered, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Recovered")
ggplot(China, aes(x=ObservationDate, y=Recovered, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Recovered")

####Hubei
Hubei<- COVID19 %>%
  filter(Province.State=="Hubei")
Hubei
plot(Hubei$Confirmed~Hubei$ObservationDate,type="l",lty=1)
lines(Hubei$Deaths~Hubei$ObservationDate,col="red",lty=2)
lines(Hubei$Recovered~Hubei$ObservationDate,col="blue",lty=3)
title("Hubei, China",lwd=3)
legend("topleft",cex=.5,c("Confirmed","Deaths","Recovered"),col=c("black","red","blue"),lty=1:3)


###Italy
Italy<-COVID19 %>%
  filter(Country.Region=="Italy")
Italy$Province.State


#Italy cases
library(dplyr)
ggplot(data=Italy)+
  geom_point(mapping=aes(x=ObservationDate, y=Confirmed, color=Province.State))+
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(size = 14))

ggplot(Italy, aes(x=as.numeric(ObservationDate), y=Confirmed, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Confirmed")
ggplot(Italy, aes(x=ObservationDate, y=Confirmed, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Confirmed")


ggplot(Italy, aes(x=as.numeric(ObservationDate), y=Deaths, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Deaths")
ggplot(Italy, aes(x=ObservationDate, y=Deaths, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Deaths")

ggplot(Italy, aes(x=as.numeric(ObservationDate), y=Recovered, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Recovered")
ggplot(Italy, aes(x=ObservationDate, y=Recovered, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Recovered")

#Italy C-D-R
Itconfirmed<-aggregate(Italy$Confirmed, by=list(ObservationDate=Italy$ObservationDate), FUN=sum)
colnames(Itconfirmed)[colnames(Itconfirmed)=="x"] <- "nationalconfirmed"
Itconfirmed

Itdeaths<-aggregate(Italy$Deaths, by=list(ObservationDate=Italy$ObservationDate), FUN=sum)
colnames(Itdeaths)[colnames(Itdeaths)=="x"] <- "nationaldeaths"
Itdeaths

Itcases<-left_join(Itconfirmed, Itdeaths, by="ObservationDate")
Itcases

Itrecovered<-aggregate(Italy$Recovered, by=list(ObservationDate=Italy$ObservationDate), FUN=sum)
colnames(Itrecovered)[colnames(Itrecovered)=="x"] <- "nationalrecovered"
Itrecovered

Itcases<-left_join(Itcases, Itrecovered, by="ObservationDate")
Itcases



ggplot()+geom_line(data = Itcases,aes(x = ObservationDate,y = nationalconfirmed, colour = "nationalconfirmed"),size=1)+
  ylim(0,303000 )+
  geom_line(data = Itcases,aes(x = ObservationDate ,y = nationaldeaths,colour ="nationaldeaths"),size=1) + 
  geom_line(data = Itcases,aes(x = ObservationDate ,y = nationalrecovered,colour ="nationalrecovered"),size=1) + 
  scale_colour_manual("",values = c("nationalconfirmed" = "black","nationaldeaths"="red","nationalrecovered" = "green"))+
  xlab("Obervation Date")+ylab("Case Number")

plot(Itcases$nationalconfirmed~Itcases$ObservationDate,type="l",lty=1)
lines(Itcases$nationaldeaths~Itcases$ObservationDate,col="red",lty=2)
lines(Itcases$nationalrecovered~Itcases$ObservationDate,col="blue",lty=3)
title("number of cases, Italy",lwd=3)
legend("topleft",cex=.5,c("Confirmed","Deaths","Recovered"),col=c("black","red","blue"),lty=1:3)



#UK cases

UK<-COVID19 %>%
  filter(Country.Region=="UK")
UK$Province.State

library(dplyr)


ggplot(UK, aes(x=as.numeric(ObservationDate), y=Confirmed, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Confirmed")
ggplot(UK, aes(x=ObservationDate, y=Confirmed, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Confirmed")


ggplot(UK, aes(x=as.numeric(ObservationDate), y=Deaths, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Deaths")
ggplot(UK, aes(x=ObservationDate, y=Deaths, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Deaths")

ggplot(UK, aes(x=as.numeric(ObservationDate), y=Recovered, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Recovered")
ggplot(UK, aes(x=ObservationDate, y=Recovered, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Recovered")

UKconfirmed<-aggregate(UK$Confirmed, by=list(ObservationDate=UK$ObservationDate), FUN=sum)

colnames(UKconfirmed)[colnames(UKconfirmed)=="x"] <- "nationalconfirmed"
UKconfirmed

UKdeaths<-aggregate(UK$Deaths, by=list(ObservationDate=UK$ObservationDate), FUN=sum)

colnames(UKdeaths)[colnames(UKdeaths)=="x"] <- "nationaldeaths"
UKdeaths
UKcases<-left_join(UKconfirmed, UKdeaths, by="ObservationDate")
UKcases

UKrecovered<-aggregate(UK$Recovered, by=list(ObservationDate=UK$ObservationDate), FUN=sum)

colnames(UKrecovered)[colnames(UKrecovered)=="x"] <- "nationalrecovered"
UKrecovered

UKcases<-left_join(UKcases, UKrecovered, by="ObservationDate")
UKcases



ggplot()+geom_line(data = UKcases,aes(x = ObservationDate,y = nationalconfirmed, colour = "nationalconfirmed"),size=1)+
  ylim(0,413000)+
  geom_line(data = UKcases,aes(x = ObservationDate ,y = nationaldeaths,colour ="nationaldeaths"),size=1) + 
  geom_line(data = UKcases,aes(x = ObservationDate ,y = nationalrecovered,colour ="nationalrecovered"),size=1) + 
  scale_colour_manual("",values = c("nationalconfirmed" = "black","nationaldeaths"="red","nationalrecovered" = "green"))+
  xlab("Obervation Date")+ylab("Case Number")


##France


France<-COVID19 %>%
  filter(Country.Region=="France")
France$Province.State

library(dplyr)


ggplot(France, aes(x=as.numeric(ObservationDate), y=Confirmed, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Confirmed")
ggplot(France, aes(x=ObservationDate, y=Confirmed, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Confirmed")


ggplot(France, aes(x=as.numeric(ObservationDate), y=Deaths, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Deaths")
ggplot(France, aes(x=ObservationDate, y=Deaths, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Deaths")

ggplot(France, aes(x=as.numeric(ObservationDate), y=Recovered, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Recovered")
ggplot(France, aes(x=ObservationDate, y=Recovered, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Recovered")


Franceconfirmed<-aggregate(France$Confirmed, by=list(ObservationDate=France$ObservationDate), FUN=sum)

colnames(Franceconfirmed)[colnames(Franceconfirmed)=="x"] <- "nationalconfirmed"
Franceconfirmed

Francedeaths<-aggregate(France$Deaths, by=list(ObservationDate=France$ObservationDate), FUN=sum)

colnames(Francedeaths)[colnames(Francedeaths)=="x"] <- "nationaldeaths"
Francedeaths
Francecases<-left_join(Franceconfirmed, Francedeaths, by="ObservationDate")
Francecases

Francerecovered<-aggregate(France$Recovered, by=list(ObservationDate=France$ObservationDate), FUN=sum)

colnames(Francerecovered)[colnames(Francerecovered)=="x"] <- "nationalrecovered"
Francerecovered

Francecases<-left_join(Francecases, Francerecovered, by="ObservationDate")
Francecases



ggplot()+geom_line(data = Francecases,aes(x = ObservationDate,y = nationalconfirmed, colour = "nationalconfirmed"),size=1)+
  ylim(0,509000)+
  geom_line(data = Francecases,aes(x = ObservationDate ,y = nationaldeaths,colour ="nationaldeaths"),size=1) + 
  geom_line(data = Francecases,aes(x = ObservationDate ,y = nationalrecovered,colour ="nationalrecovered"),size=1) + 
  scale_colour_manual("",values = c("nationalconfirmed" = "black","nationaldeaths"="red","nationalrecovered" = "green"))+
  xlab("Obervation Date")+ylab("Case Number")


##US

US<-COVID19 %>%
  filter(Country.Region=="US")
summary(US$Province.State)

USfinal<- US %>%
  filter(ObservationDate=="2020-09-23")


stateconfirmed<-aggregate(USfinal$Confirmed, by=list(state=USfinal$Province.State), FUN=sum)
colnames(stateconfirmed)[colnames(stateconfirmed)=="x"] <- "stateconfirmedfinal"

stateconfirmed%>%
   filter(state=="New York")
stateconfirmed%>%
   filter(state=="California")
stateconfirmed%>%
  filter(state=="Florida")
stateconfirmed%>%
  filter(state=="Texas")
library(sf)
library(usmap)

plot_usmap(regions="state", data=stateconfirmed, values="stateconfirmedfinal")+
  labs(title="number of all confirmed cases in US till 09/23")


statedeaths<-aggregate(USfinal$Deaths, by=list(state=USfinal$Province.State), FUN=sum)
colnames(statedeaths)[colnames(statedeaths)=="x"] <- "statedeathsfinal"

statedeaths

statedeaths%>%
  filter(state=="New York")
statedeaths%>%
  filter(state=="California")
statedeaths%>%
  filter(state=="Florida")
statedeaths%>%
  filter(state=="Texas")
statedeaths%>%
  filter(state=="Illinois")

plot_usmap(regions="state", data=statedeaths, values="statedeathsfinal")+
  labs(title="number of all deaths in US till 09/23")


USconfirmed<-aggregate(US$Confirmed, by=list(ObservationDate=US$ObservationDate), FUN=sum)

colnames(USconfirmed)[colnames(USconfirmed)=="x"] <- "nationalconfirmed"
USconfirmed

USdeaths<-aggregate(US$Deaths, by=list(ObservationDate=US$ObservationDate), FUN=sum)

colnames(USdeaths)[colnames(USdeaths)=="x"] <- "nationaldeaths"
USdeaths
UScases<-left_join(USconfirmed, USdeaths, by="ObservationDate")
UScases

USrecovered<-aggregate(US$Recovered, by=list(ObservationDate=US$ObservationDate), FUN=sum)

colnames(USrecovered)[colnames(USrecovered)=="x"] <- "nationalrecovered"
USrecovered

UScases<-left_join(UScases, USrecovered, by="ObservationDate")
UScases

ggplot()+geom_line(data = UScases,aes(x = ObservationDate,y = nationalconfirmed, colour = "nationalconfirmed"),size=1)+
  ylim(0,7000000)+
  geom_line(data = UScases,aes(x = ObservationDate ,y = nationaldeaths,colour ="nationaldeaths"),size=1) + 
  geom_line(data = UScases,aes(x = ObservationDate ,y = nationalrecovered,colour ="nationalrecovered"),size=1) + 
  scale_colour_manual("",values = c("nationalconfirmed" = "black","nationaldeaths"="red","nationalrecovered" = "green"))+
  xlab("Obervation Date")+ylab("Case Number")





## Australia



Australia<-COVID19 %>%
  filter(Country.Region=="Australia")
summary(Australia$Province.State)

library(dplyr)


ggplot(Australia, aes(x=as.numeric(ObservationDate), y=Confirmed, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Confirmed")
ggplot(Australia, aes(x=ObservationDate, y=Confirmed, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Confirmed")


ggplot(Australia, aes(x=as.numeric(ObservationDate), y=Deaths, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Deaths")
ggplot(Australia, aes(x=ObservationDate, y=Deaths, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Deaths")

ggplot(Australia, aes(x=as.numeric(ObservationDate), y=Recovered, color=Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Recovered")
ggplot(Australia, aes(x=ObservationDate, y=Recovered, color=Province.State, Province.State)) +
  geom_point()+
  geom_line() + 
  labs(x="ObservationDate", y="Recovered")


Auconfirmed<-aggregate(Australia$Confirmed, by=list(ObservationDate=Australia$ObservationDate), FUN=sum)

colnames(Auconfirmed)[colnames(Auconfirmed)=="x"] <- "nationalconfirmed"
Auconfirmed

Audeaths<-aggregate(Australia$Deaths, by=list(ObservationDate=Australia$ObservationDate), FUN=sum)

colnames(Audeaths)[colnames(Audeaths)=="x"] <- "nationaldeaths"
Audeaths
Aucases<-left_join(Auconfirmed, Audeaths, by="ObservationDate")
Aucases

Aurecovered<-aggregate(Australia$Recovered, by=list(ObservationDate=Australia$ObservationDate), FUN=sum)

colnames(Aurecovered)[colnames(Aurecovered)=="x"] <- "nationalrecovered"
Aurecovered

Aucases<-left_join(Aucases, Aurecovered, by="ObservationDate")
Aucases



ggplot()+geom_line(data = Aucases,aes(x = ObservationDate,y = nationalconfirmed, colour = "nationalconfirmed"),size=1)+
  ylim(0,27000)+
  geom_line(data = Aucases,aes(x = ObservationDate ,y = nationaldeaths,colour ="nationaldeaths"),size=1) + 
  geom_line(data = Aucases,aes(x = ObservationDate ,y = nationalrecovered,colour ="nationalrecovered"),size=1) + 
  scale_colour_manual("",values = c("nationalconfirmed" = "black","nationaldeaths"="red","nationalrecovered" = "green"))+
  xlab("Obervation Date")+ylab("Case Number")
















#### world map

summary(COVID19$Country.Region)
library(dplyr)
worldfinal<-COVID19 %>%
  filter(ObservationDate=="2020-09-23")
library(tidyr)
worldfinal 
worldfinal$country <- recode(worldfinal$Country.Region
                             ,'Mainland China' = 'China')

worldfinal$country

library(RColorBrewer)
library(rworldmap)

head(worldfinal)
worldfinalConfirmed<-aggregate(worldfinal$Confirmed, by=list(country=worldfinal$country), FUN=sum)
colnames(worldfinalConfirmed)[colnames(worldfinalConfirmed)=="x"] <- "World Daily Confirmed Cases"

worldfinalConfirmed
worldMapConfirmed<- joinCountryData2Map(worldfinalConfirmed,
                                        nameJoinColumn = "country",
                                        joinCode="NAME")

colourPalette<-RColorBrewer::brewer.pal(10,'Spectral')

mapCountryData(worldMapConfirmed,
               nameColumnToPlot = 'World Daily Confirmed Cases',
               catMethod = 'fixedwidth',
               colourPalette = colourPalette,
               numCats=100)


worldfinalDeaths<-aggregate(worldfinal$Deaths, by=list(country=worldfinal$country), FUN=sum)
colnames(worldfinalDeaths)[colnames(worldfinalDeaths)=="x"] <- "World Daily Deaths"

worldfinalDeaths
worldMapDeaths<- joinCountryData2Map(worldfinalDeaths,
                                        nameJoinColumn = "country",
                                        joinCode="NAME")

colourPalette<-RColorBrewer::brewer.pal(10,'Spectral')

mapCountryData(worldMapDeaths,
               nameColumnToPlot = 'World Daily Deaths',
               catMethod = 'fixedwidth',
               colourPalette = colourPalette,
               numCats=100)

#worldcases1<-left_join(worldfinalConfirmed, worldfinalDeaths, by="country")
#worldcases1<-worldcases1 %>%          
#  mutate(CFR = 'World Daily Deaths'/ 'World Daily Confirmed Cases') 
#worldcases1

#### CFR 09/23

worldconfirmed<-read.csv("time_series_covid_19_confirmed.csv")

#world confirmed
worldconfirmed<-worldconfirmed %>%
  select(Country.Region,X9.23.20)
worldconfirmed$country<- worldconfirmed$Country.Region
worldconfirmed<-worldconfirmed %>%
  select(country,X9.23.20)
worldconfirmed$confirmed<- worldconfirmed$X9.23.20
worldconfirmed<-worldconfirmed %>%
  select(country,confirmed)
worldconfirmed
worldconfirmed<-aggregate(worldconfirmed$confirmed, by=list(country=worldconfirmed$country), FUN=sum)
worldconfirmed
colnames(worldconfirmed)[colnames(worldconfirmed)=="x"] <- "confirmed"

worldconfirmed
summary(worldconfirmed$country)
##world deaths
worlddeaths<-read.csv("time_series_covid_19_deaths.csv")
worlddeaths
worlddeaths<-worlddeaths %>%
  select(Country.Region,X9.23.20)
worlddeaths$country<- worlddeaths$Country.Region
worlddeaths<-worlddeaths %>%
  select(country,X9.23.20)
worlddeaths$deaths<- worlddeaths$X9.23.20
worlddeaths<-worlddeaths %>%
  select(country,deaths)
worlddeaths
worlddeaths<-aggregate(worlddeaths$deaths, by=list(country=worlddeaths$country), FUN=sum)
worlddeaths
colnames(worlddeaths)[colnames(worlddeaths)=="x"] <- "deaths"

worldconfirmed
worlddeaths

worlddeaths
worldfinaldeaths<- joinCountryData2Map(worlddeaths,
                                       nameJoinColumn = "country",
                                       joinCode="NAME")

colourPalette<-RColorBrewer::brewer.pal(10,'Spectral')

mapCountryData(worldfinaldeaths,
               nameColumnToPlot = 'deaths',
               catMethod = 'fixedwidth',e
               colourPalette = colourPalette,
               numCats=100)

worlddeaths %>%
  filter(country=="Brazil")

worlddeaths %>%
  filter(country=="India")

worlddeaths %>%
  filter(country=="Mexico")




worldcases<-left_join(worldconfirmed, worlddeaths, by="country")
worldcases<-worldcases %>%          
  mutate(CFR = deaths / confirmed) 
worldCFR<-worldcases %>%
  select(country, CFR)
worldCFR

worldMapCFR<- joinCountryData2Map(worldCFR,
                                     nameJoinColumn = "country",
                                     joinCode="NAME")

colourPalette<-RColorBrewer::brewer.pal(10,'Spectral')

mapCountryData(worldMapCFR,
               nameColumnToPlot = 'CFR',
               catMethod = 'fixedwidth',
               colourPalette = colourPalette,
               numCats=100)
####Yeman
worldCFR1<- worldCFR %>%
  filter(country=="Yemen")
worldCFR1

## CFR 08/23
worldconfirmed<-read.csv("time_series_covid_19_confirmed.csv")

#world confirmed
worldconfirmed<-worldconfirmed %>%
  select(Country.Region,X8.23.20)
worldconfirmed$country<- worldconfirmed$Country.Region
worldconfirmed<-worldconfirmed %>%
  select(country,X8.23.20)
worldconfirmed$confirmed<- worldconfirmed$X8.23.20
worldconfirmed<-worldconfirmed %>%
  select(country,confirmed)
worldconfirmed
worldconfirmed<-aggregate(worldconfirmed$confirmed, by=list(country=worldconfirmed$country), FUN=sum)
worldconfirmed
colnames(worldconfirmed)[colnames(worldconfirmed)=="x"] <- "confirmed"

worldconfirmed
summary(worldconfirmed$country)
##world deaths
worlddeaths<-read.csv("time_series_covid_19_deaths.csv")
worlddeaths
worlddeaths<-worlddeaths %>%
  select(Country.Region,X8.23.20)
worlddeaths$country<- worlddeaths$Country.Region
worlddeaths<-worlddeaths %>%
  select(country,X8.23.20)
worlddeaths$deaths<- worlddeaths$X8.23.20
worlddeaths<-worlddeaths %>%
  select(country,deaths)
worlddeaths
worlddeaths<-aggregate(worlddeaths$deaths, by=list(country=worlddeaths$country), FUN=sum)
worlddeaths
colnames(worlddeaths)[colnames(worlddeaths)=="x"] <- "deaths"




worlddeaths %>%
  filter(country=="Brazil")

worlddeaths %>%
  filter(country=="India")

worlddeaths %>%
  filter(country=="Mexico")



worldconfirmed
worlddeaths
worldcases<-left_join(worldconfirmed, worlddeaths, by="country")
worldcases<-worldcases %>%          
  mutate(CFR = deaths / confirmed) 
worldCFR<-worldcases %>%
  select(country, CFR)
worldCFR

worldMapCFR<- joinCountryData2Map(worldCFR,
                                  nameJoinColumn = "country",
                                  joinCode="NAME")

colourPalette<-RColorBrewer::brewer.pal(10,'Spectral')

mapCountryData(worldMapCFR,
               nameColumnToPlot = 'CFR',
               catMethod = 'fixedwidth',
               colourPalette = colourPalette,
               numCats=100)




#iNDIA
India<-COVID19 %>%
  filter(Country.Region=="India")

Indiaconfirmed<-aggregate(India$Confirmed, by=list(ObservationDate=India$ObservationDate), FUN=sum)

colnames(Indiaconfirmed)[colnames(Indiaconfirmed)=="x"] <- "nationalconfirmed"
Indiaconfirmed

Brazil<-COVID19 %>%
  filter(Country.Region=="Brazil")

Brazilconfirmed<-aggregate(Brazil$Confirmed, by=list(ObservationDate=Brazil$ObservationDate), FUN=sum)

colnames(Brazilconfirmed)[colnames(Brazilconfirmed)=="x"] <- "nationalconfirmed"
Brazilconfirmed




Indiadeaths<-aggregate(India$Deaths, by=list(ObservationDate=India$ObservationDate), FUN=sum)

colnames(Indiadeaths)[colnames(Indiadeaths)=="x"] <- "nationaldeaths"
Indiadeaths



Brazildeaths<-aggregate(Brazil$Deaths, by=list(ObservationDate=Brazil$ObservationDate), FUN=sum)

colnames(Brazildeaths)[colnames(Brazildeaths)=="x"] <- "nationaldeaths"
Brazildeaths


Mexico<-COVID19 %>%
  filter(Country.Region=="Mexico")


Mexideaths<-aggregate(Mexico$Deaths, by=list(ObservationDate=Mexico$ObservationDate), FUN=sum)

colnames(Mexideaths)[colnames(Mexideaths)=="x"] <- "nationaldeaths"
Mexideaths





