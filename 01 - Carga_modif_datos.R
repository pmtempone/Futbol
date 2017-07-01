----#carga de librerias#-----

library(FactoMineR)
library(foreign)
library(psych)
library(knitr)
library(xtable)
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(dendextendRcpp))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggfortify))
library(corrplot)
#library(Stuff)
suppressPackageStartupMessages(library(dplyr))
library(broom)
library(plotly)
library(reshape)
library(GGally)
library(lubridate)
library(SportsAnalytics)
library(archetypes)
library("RColorBrewer")

----#carga de sets de datos#-----

DatosPremiumArgentina2013 <- read.csv("DatosPremiumArgentina2013.csv", sep=";")
DatosPremiumArgentina2014 <- read.csv("DatosPremiumArgentina2014.csv", sep=";")
DatosPremiumArgentina2015 <- read.csv("DatosPremiumArgentina2015.csv", sep=";")
DatosPremiumArgentina2016 <- read.csv("DatosPremiumArgentina2016.csv", sep=";")

DatosPremiumArgentina2013[,c("local","visitante","perso_nombre","perso_apellido","team")] <- NULL
DatosPremiumArgentina2014[,c("local","visitante","perso_nombre","perso_apellido","team")] <- NULL
DatosPremiumArgentina2015[,c("local","visitante","perso_nombre","perso_apellido","team")] <- NULL
DatosPremiumArgentina2016[,c("local","visitante","perso_nombre","perso_apellido","team")] <- NULL

'
DatosPremiumArgentina2016$rol_id_rol <- factor(DatosPremiumArgentina2016$rol_id_rol)

summary(DatosPremiumArgentina2016)

DatosPremiumArgentina2016$local.1 <- as.character(DatosPremiumArgentina2016$local.1)

DatosPremiumArgentina2016$team.1 <- as.character(DatosPremiumArgentina2016$team.1)


DatosPremiumArgentina2016$J_local <- ifelse(DatosPremiumArgentina2016$team.1==DatosPremiumArgentina2016$local.1,"L","V")

ggplot(data = DatosPremiumArgentina2016, aes(x = J_local,y=disparo_afuera, fill = J_local)) + geom_boxplot()
ggplot(data = DatosPremiumArgentina2016, aes(x = J_local,y=pase_correcto, fill = J_local)) + geom_boxplot()


datos2016 <- DatosPremiumArgentina2016[DatosPremiumArgentina2016$team.1!
'

-----#unificacion de bases------

Basetotal <- rbind(DatosPremiumArgentina2013,DatosPremiumArgentina2014,DatosPremiumArgentina2015,DatosPremiumArgentina2016)

---#correccion tipo de datos-------

Basetotal$rol_id_rol <- factor(Basetotal$rol_id_rol)

Basetotal$local.1 <- as.character(Basetotal$local.1)

Basetotal$team.1 <- as.character(Basetotal$team.1)

----# creacion variable local visitante-----
Basetotal$J_local <- ifelse(Basetotal$team.1==Basetotal$local.1,"L","V")

-----#se saca la seleccion y el apodo----
Basetotal <- Basetotal[Basetotal$team.1!='Argentina',]
Basetotal$perso_apodo <- NULL
Basetotal <- Basetotal[Basetotal$rol_id_rol %in% c('1','2','3','4'),] # se sacan tecnicos y arbitros

summary(Basetotal)

-----#transformacion datos----

dat.m <- melt(Basetotal,id.vars='J_local', measure.vars=c("goles_convertidos","asistencias","disparo_afuera","disparo_palo","disparo_atajado","penal_errado","faltas","offsides","amarillas","doble_amarilla","rojas","pase_correcto","pase_incorrecto","despejes","quites","atajadas","atajada_penal"))

----#multboxplot----
p <- ggplot(dat.m) + geom_boxplot(aes(x=J_local, y=value, color=variable))

p+facet_wrap( ~ variable, scales="free")

----#grafico de barras local vs visitante de rol id y titular-----
t1 <- count(Basetotal,vars = c("J_local","rol_id_rol"))

p1 <- ggplot(t1,aes(x=rol_id_rol,y=freq,fill=J_local))+geom_bar(stat="identity",position="dodge")

t2 <- count(Basetotal,vars = c("J_local","titular"))

p2 <- ggplot(t2,aes(x=titular,y=freq,fill=J_local))+geom_bar(stat="identity",position="dodge")

multiplot(p1,p2)

----#Cambio a tipo fecha-----
Basetotal$fecha <- as.Date(Basetotal$fecha,"%d/%m/%Y")

l1 <- ggplot(Basetotal,aes(x=fecha,y=goles_convertidos))+geom_line()

l1

#----#datos por jugador----

jugadores <- Basetotal %>% dplyr::select(perso_nombre.1,perso_apellido.1,minutos_jugados:atajada_penal)%>%group_by(perso_nombre.1,perso_apellido.1)

jugadores_agr <- jugadores %>%summarise_each(funs(sum))  

#se sacan jugadores con menos de 90 minutos en los 3 torneos

jugadores_agr <- jugadores_agr %>% filter(minutos_jugados>=90)

jugadores_agr <- jugadores_agr %>% mutate(pr_goles_convertidos=goles_convertidos/minutos_jugados,pr_asistencias=asistencias/minutos_jugados,
           pr_disparo_afuera=disparo_afuera/minutos_jugados,pr_disparo_atajado=disparo_atajado/minutos_jugados,
           pr_faltas=faltas/minutos_jugados,pr_faltas_recibidos=faltas_recibidas/minutos_jugados,pr_offsides=offsides/minutos_jugados,
           pr_amarillas=(amarillas+doble_amarilla)/minutos_jugados,pr_expulsados=(doble_amarilla+rojas)/minutos_jugados,pr_pase_correcto=pase_correcto/minutos_jugados,
           pr_incorrecto=pase_incorrecto/minutos_jugados,pr_despejes=despejes/minutos_jugados,pr_quites=quites/minutos_jugados,pr_atajadas=atajadas/minutos_jugados,jugador=paste(perso_apellido.1,perso_nombre.1,sep=","))




