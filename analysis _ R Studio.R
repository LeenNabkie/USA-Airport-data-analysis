####################################################################################################################################

####################################################################################################################################
#       0. importez les données et les nettoyez    
####################################################################################################################################



flights= read.csv(choose.files(),header= TRUE, sep=",")

airlines=read.csv(choose.files(),header= TRUE, sep=",")
airports=read.csv(choose.files(),header= TRUE, sep=",")

# Ou bien:

setwd("C:/Users/gita/Desktop/HEC_COURS/ZCH21/ATL2021/BD")
getwd()

flights= read.csv("flights.csv",header= TRUE, sep=",")
airlines=read.csv("airlines.csv",header= TRUE, sep=",")
airports=read.csv("airports.csv",header= TRUE, sep=",")

df0=flights[,-c(6,7,13:20)] # enlever les colonnes qui ne sont pas nécessaires


####################################################################################################################################
#       0.1. filtrez les deux aéroports    
####################################################################################################################################
airports[airports$CITY=="Boston",]

airports[airports$CITY=="Boston",c(1,3)] # IATA_CODE de l'aeroport de Boston est "BOS"
airports[airports$CITY=="Los Angeles",c(1,3)] # IATA_CODE de l'aeroport de Los Angeles est "LAX"

df1=df0[df0$ORIGIN_AIRPORT=="BOS"& df0$DESTINATION_AIRPORT=="LAX",]

#list_unique_group=unique(airports[,c(4)])


####################################################################################################################################
# 0.2. quelques statistiques descriptives 
####################################################################################################################################

summary(df1)

sum(is.na(df1$YEAR))
sum(is.na(df1$MONTH))
sum(is.na(df1$DAY))
sum(is.na(df1$DAY_OF_WEEK))
sum(is.na(df1$MONTH))

unique(df1$YEAR)
table(df1$YEAR)


unique(df1$MONTH)
table(df1$MONTH)
t1=as.data.frame(table(df1$MONTH))
t1$Var1=as.numeric(as.character(t1$Var1))
class(t1$Var1)
prop.table(t1) 
prop.table(t1,t1$Freq)


unique(df1$DAY_OF_WEEK)
table(df1$DAY_OF_WEEK)



####################################################################################################################################
# 0.3 remplacez les "NA"
####################################################################################################################################

sum(is.na(df1$DEPARTURE_DELAY))
df1$DEPARTURE_DELAY[is.na(df1$DEPARTURE_DELAY)]=0 
# on remplace « NA » avec 0, car « NA » ici l'information est indisponible. 
#Une manière de se débarrasser de ce problème est de remplacer ces « N » par 0.

sum(is.na(df1$DEPARTURE_DELAY))
####################################################################################################################################
# Q1:Quelle est la journée qui enregistre le plus/ le moins de délais au départ ? 
####################################################################################################################################
 
## 1.1. créer une colonne pour définir les jours: 
df1$Day_NAME=ifelse(df1$DAY_OF_WEEK==1,"Sunday",
                    ifelse(df1$DAY_OF_WEEK==2,"Monday",
                           ifelse(df1$DAY_OF_WEEK==3,"Tuesday",
                                  ifelse(df1$DAY_OF_WEEK==4,"Wednesday",
                                         ifelse(df1$DAY_OF_WEEK==5,"Thursday",
                                                ifelse(df1$DAY_OF_WEEK==6,"Friday","Saterday"))))))


## 1.2. agrégez les données:
Jour_Delai_somme=aggregate(df1[,c(10)],by=list(df1$Day_NAME),FUN="sum")

Jour_Delai_somme=Jour_Delai_somme[with(Jour_Delai_somme, order(x)), ]

head(Jour_Delai_somme,1)

tail(Jour_Delai_somme,1)

# Samedi est la journée qui enregistre le plus de délais au départ et le Vendredi est la journée enregistrant le moins. 
# Il est important de remarquer que plusieurs vols pourraient avoir lieu le Samedi par rapport à vendredi et donc nous
#avons plus de chance d'occasionner un retard ce jour-là.
#La meilleure façon d'étudier les retards serait de travailler avec le délai au départ moyen.

Jour_Delai_moyen=aggregate(df1[,c(10)],by=list(df1$Day_NAME),FUN="mean")


Jour_Delai_moyen=Jour_Delai_moyen[with(Jour_Delai_moyen, order(x)), ]

head(Jour_Delai_moyen,1)
tail(Jour_Delai_moyen,1)
####################################################################################################################################
# Q2:Quel mois on a moins et plus d'annulations en moyenne : 
# on veut que votre table soit parâtre dans l'environnement global et aussi vous l'exporter en formate CSV:
####################################################################################################################################

## 2.1. créer une table dans l'environnement global:

Mois_annulation_moyen=aggregate(df1[,c(15)],by=list(df1$MONTH),FUN="mean")

Mois_annulation_moyen=Mois_annulation_moyen[with(Mois_annulation_moyen, order(x)), ]

head(Mois_annulation_moyen,1)
tail(Mois_annulation_moyen,1)
list_unique_mois=unique(Mois_annulation_moyen[,1])

## réponse: 
# Les mois occasionnant peu d'annulations en moyenne sont mars, avril, mai, novembre et decembre.
# Le mois qui enregistre le plus d'annulations en moyenne est février.

## 2.2. exportez la table en format csv:
setwd("C:\\Users\\gita\\Desktop\\HEC_COURS\\ZCH21\\ATL2021\\BD") 
write.csv(Mois_annulation_moyen,file="Mois_annulation_moyen.csv",row.names = TRUE)



####################################################################################################################################
# Q3.	Quelles serait la meilleure/la pire compagnie aérienne en considérant le critère d'annulations
#     et par la suite en considérant le critère du délai au départ? 
####################################################################################################################################

# 3.1. dans un premier temps il faut joindre les deux tables de données.
# changer le nom de la colonne "Aireline" à "IATA_CODE" pour éviter des erreurs en fusionnant les deux tables


names(df1)[5]="IATA_CODE"  
df2=merge(df1,airlines,by="IATA_CODE",all.x=T)

list_unique_IATA_CODE=unique(df2[,1])


summary(df2$IATA_CODE)

## 3.2. Aggréger les données:
com_airline=aggregate(df2[,c(15)],by=list(df2$AIRLINE),FUN="mean")

com_airline=com_airline[with(com_airline, order(x)), ]

head(com_airline,1)
tail(com_airline,1)

## Réponse: La meilleure compagnie est "Delta Air Lines Inc." et la pire est "Virgin America". 
## Suivez la même démarche pour le critère « délai au départ ».



####################################################################################################################################
# Q4.	Quelle est la raison d'annulation plus fréquente?
####################################################################################################################################



summary(df2$CANCELLATION_REASON)

## Réponse : B



####################################################################################################################################
# Q5.Présentez graphiquement le délai moyen au départ par mois, en considérant l'année 2015.
####################################################################################################################################


## 5.1. créer le colonne de nom des mois:
df2$MONTH_NAME=ifelse(df2$MONTH==1,"January",
                      ifelse(df2$MONTH==2,"February",
                             ifelse(df2$MONTH==3,"March",
                                    ifelse(df2$MONTH==4,"April",
                                           ifelse(df2$MONTH==5,"May",
                                                  ifelse(df2$MONTH==6,"Jun",
                                                         ifelse(df2$MONTH==7,"July",
                                                                ifelse(df2$MONTH==8,"Augest",
                                                                       ifelse(df2$MONTH==9,"September",
                                                                              ifelse(df2$MONTH==10,"October",
                                                                                     ifelse(df2$MONTH==11,"November","December")
                                                                              )
                                                                       )
                                                                       
                                                                )
                                                                
                                                         )
                                                         
                                                  )
                                                  
                                           )
                                           
                                    )
                                    
                             )
                             
                      )
)

## 5.2. installer le package ggplot pour effectuer la présentation graphique
install.packages("ggplot2")
library(ggplot2)

## 5.3. Graphique: 


ggplot(df2,aes( y=df2$DEPARTURE_DELAY, x = df2$MONTH_NAME)) + 
  geom_boxplot(na.rm = TRUE, colour="#00aaff") + 
  xlab("Mois") + ylab("Délai de Départ")+ylim(0,30)+
  ggtitle(paste0("Mois",' en fonction de ',"Délai de Départ"))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))




####################################################################################################################################
# Q6.Créez une colonne de date
####################################################################################################################################


df2$date <- as.Date(with(df2, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")
df2$date
class(df2$date)



####################################################################################################################################
# Q7.Créez un échantillon aléatoire de 100000 du fichier "Flights"
####################################################################################################################################

set.seed(470600)
samplesize=100000
x=sample(1:nrow(flights),samplesize)
df5=flights[x,]

