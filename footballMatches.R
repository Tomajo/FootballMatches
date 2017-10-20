library(dplyr)
library(stringr)
setwd('/home/toni/Projectes/R/FootballMatchesSpanishLeague/')
dadesLliga <- read.csv("data/FMEL_Dataset.csv") 

####
####  Data Quality
####
equipsLocals<-select(dadesLliga,localTeam)%>%distinct()%>%arrange(localTeam)


#Veig una cosa rara amb el  almeria i el atletic de madrid 
filter(dadesLliga,localTeam=='Almeria')%>%filter(str_detect(visitorTeam, "A"))
filter(dadesLliga,localTeam=='Atletico de Madrid')%>%filter(str_detect(visitorTeam, "Atl"))
#Sembla que estàa tot bé.
#

#Començo a asajar amb el tema de contar els partits guanyats per temporada, jornada a jornada, mes els gols a favor i en contra etc....

#faig un sum dels gols a favor i en contra com a local
local<-select(dadesLliga,season,localTeam,visitorTeam,localGoals,visitorGoals) %>% 
    group_by(localTeam,season) %>% 
    summarise(golsCasaFavor=sum(localGoals),golsCasaEncaixats=sum(visitorGoals))
#faig un sum dels gols a favor i en contra com a visitant
visitant<-select(dadesLliga,season,localTeam,visitorTeam,localGoals,visitorGoals) %>% 
    group_by(visitorTeam,season) %>% 
    summarise(golsVisitantFavor=sum(visitorGoals),golsVisitantEncaixats=sum(localGoals))

#faig un  join per tenir les dues visions 
golsFavorContra<-inner_join(local,visitant,by=c("localTeam"="visitorTeam",c("season"="season")))


#TODO: Ara que tinc axiò cal provar anar fent el mateix per jornades 
#i anar sumant i tenir per cada jornada els gols a favor i en contra de 
#cada equip en una jornada donada.



###### Creo columna nova amb el any de la temporada per poder comparar dates. Ho poso en númeric. Si es 1980-81 paso a 1980.
dadesLliga$temporada<-as.numeric(substr(dadesLliga$season,1,4))

#Faig camp equip per fer els calculs a dins el bucle
dadesLliga$equip<-as.numeric(dadesLliga$localTeam)

dadesLliga["golsCasaFavor"]<-NA
dadesLliga["golsCasaEncaixats"]<-NA
for(i in 1976:2016){
    for (j in unique(dadesLliga[dadesLliga$temporada==i,]$round)){
        print(c("La Temporada es: ", i, " La Jornada es ", j))
        for(k in (dadesLliga[dadesLliga$temporada==i & dadesLliga$round==j,]$equip)){
            gols<-select(dadesLliga,localTeam,visitorTeam,localGoals,visitorGoals,temporada,round,equip) %>% 
                filter(equip==k &(temporada<i & temporada >(i-5) | temporada==i & round<j))%>%
                group_by(equip) %>% 
                summarise(golsCasaFavor=sum(localGoals),golsCasaEncaixats=sum(visitorGoals))
            if(length(gols$golsCasaFavor)==0){
                dadesLliga[dadesLliga$temporada==i & dadesLliga$round==j & as.numeric(dadesLliga$equip)==k,]$golsCasaFavor<-0
                dadesLliga[dadesLliga$temporada==i & dadesLliga$round==j & as.numeric(dadesLliga$equip)==k,]$golsCasaEncaixats<-0
            }else{
                dadesLliga[dadesLliga$temporada==i & dadesLliga$round==j & as.numeric(dadesLliga$equip)==k,]$golsCasaFavor<-as.numeric(gols$golsCasaFavor)
                dadesLliga[dadesLliga$temporada==i & dadesLliga$round==j & as.numeric(dadesLliga$equip)==k,]$golsCasaEncaixats<-as.numeric(gols$golsCasaEncaixats)
            }
        }
    }
}

#fem proves per enumerar les jornades i així poder fer fàcil comparacions
distinct(select(dadesLliga,temporada,round))

