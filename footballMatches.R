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
dadesLliga$equipCasa<-as.numeric(dadesLliga$localTeam)
dadesLliga$equipVisitant<-as.numeric(dadesLliga$visitorTeam)



#TODO: Falta repensar com fer el compteig de gols a favor i gols en contra.
###########################################################################
#faig les mitjes dels encaixats a casa i fora i els marcats a casa i fora per cada equip
############################


mitjesTotals<-dadesLliga %>% group_by(season,division) %>% summarise(localGoals=mean(localGoals),visitorGoals=mean(visitorGoals),equipsLocalsTemporada=n_distinct(localTeam),equipsVisitantsTemporada=n_distinct(visitorTeam))
mitjesEquipsEncaixatsLocal<-dadesLliga %>% group_by(season,division,team=localTeam) %>% summarise(localGoalsEncaixats=mean(visitorGoals))
mitjesEquipsMarcatsLocal<-dadesLliga %>% group_by(season,division,team=localTeam) %>% summarise(localGoalsMarcats=mean(localGoals))
mitjesEquipsMarcatsVisitant<-dadesLliga %>% group_by(season,division,team=visitorTeam) %>% summarise(visitorGoalsMarcats=mean(visitorGoals))
mitjesEquipsEncaixatVisitant<-dadesLliga %>% group_by(season,division,team=visitorTeam) %>% summarise(visitorGoalsEncaixats=mean(localGoals))

iner1<-inner_join(mitjesTotals,mitjesEquipsEncaixatsLocal,by=c("season","division"))
iner2<-inner_join(iner1,mitjesEquipsMarcatsLocal,by=c("season","division","team"))
iner3<-inner_join(iner2,mitjesEquipsMarcatsVisitant,by=c("season","division","team"))
resultats<-inner_join(iner3,mitjesEquipsEncaixatVisitant,by=c("season","division","team"))
resultats$awayDefensiveStrength<-resultats$visitorGoalsEncaixats/resultats$localGoals
resultats$awayAttackStrength<-resultats$visitorGoalsMarcats/resultats$visitorGoals
resultats$homeDefensiveStrength<-resultats$localGoalsEncaixats/resultats$visitorGoals
resultats$homeAttackStrength<-resultats$localGoalsMarcats/resultats$localGoals
resultats$projHomeTeamGoals<-resultats$homeAttackStrength*resultats$awayDefensiveStrength*resultats$localGoals
resultats$projVisitorTeamGoals<-resultats$awayAttackStrength*resultats$homeDefensiveStrength*resultats$visitorGoals

# for(i in 1976:2016){
#     for (j in unique(dadesLliga[dadesLliga$temporada==i,]$round)){
#         print(c("La Temporada es: ", i, " La Jornada es ", j))
#  
#     }
# }




# Calcul de la matriu per un partit
#Falta iterar per tots els partits tenin en compte les dades de la temporada passada....
#revisar si els resultats tenen sentit.... amb les dades de l Barça.
dadesLliga%>%filter(str_detect(localTeam,'Barcelona'))
prob_matrix<-data.frame()
for (i in 0:10 ){      # i= Home Team Goals
    
    for (j in 0:10){      # j= Visitor Team Goals
        
        prob_matrix[i+1,j+1]<-round(dpois(i,resultats$projHomeTeamGoals[1])*dpois(j,resultats$projVisitorTeamGoals[1])*100,2)
        
    }
}
row.names(prob_matrix)<-0:10
colnames(prob_matrix)<-0:10
prob_matrix
#fem proves per enumerar les jornades i així poder fer fàcil comparacions
distinct(select(dadesLliga,temporada,round))

