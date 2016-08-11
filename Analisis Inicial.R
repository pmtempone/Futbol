library(ggplot2)
library(dplyr)
library(plyr)
library(reshape)
library(GGally)
library(lubridate)
library(SportsAnalytics)
library(archetypes)

#Analisis de datos

summary(datos2016)

head(datos2016[datos2016$rol_id_rol=='5',]) # tecnicos rol 5

datos2016 <- datos2016[datos2016$rol_id_rol!='5',]

#transformacion datos

dat.m <- melt(datos2016,id.vars='J_local', measure.vars=c("goles_convertidos","goles_encontra","asistencias","disparo_afuera","disparo_palo","disparo_atajado","penal_errado","faltas","offsides","amarillas","doble_amarilla","rojas","pase_correcto","pase_incorrecto","despejes","quites","atajadas","atajada_penal"))


#multboxplot
p <- ggplot(dat.m) + geom_boxplot(aes(x=J_local, y=value, color=variable))

p+facet_wrap( ~ variable, scales="free")

#correlaciones pases
ggpairs(datos2016,mapping = ggplot2::aes(color=J_local),columns = c("goles_convertidos","asistencias","disparo_afuera","disparo_atajado","pase_correcto","despejes","quites"))

#grafico de barras local vs visitante de rol id y titular
t1 <- count(datos2016,vars = c("J_local","rol_id_rol"))

p1 <- ggplot(t1,aes(x=rol_id_rol,y=freq,fill=J_local))+geom_bar(stat="identity",position="dodge")

t2 <- count(datos2016,vars = c("J_local","titular"))

p2 <- ggplot(t2,aes(x=titular,y=freq,fill=J_local))+geom_bar(stat="identity",position="dodge")

multiplot(p1,p2)
#head(as.Date(datos2016$fecha,"%d/%m/%Y"))
datos2016$fecha <- as.Date(datos2016$fecha,"%d/%m/%Y")

l1 <- ggplot(datos2016,aes(x=fecha,y=goles_convertidos))+geom_line(stat="identity",position="dodge")

#head(month(datos2016$fecha)) funcion de lubridate

l1 <- ggplot(datos2016,aes(x=month(fecha),y=goles_convertidos))+ geom_line() +
  xlab("") + ylab("goles convertidos")
l1

#datos por jugador

jugadores <- datos2016 %>% select(perso_nombre,perso_apellido,rol_id_rol,team,minutos_jugados:atajada_penal)%>%group_by(perso_nombre,perso_apellido,rol_id_rol,team)

jugadores_agr <- jugadores %>%summarise_each(funs(sum)) %>% mutate(pr_goles_convertidos=goles_convertidos/minutos_jugados,pr_asistencias=asistencias/minutos_jugados,
                                                                   pr_disparo_afuera=disparo_afuera/minutos_jugados,pr_disparo_atajado=disparo_atajado/minutos_jugados,
                                                                   pr_faltas=faltas/minutos_jugados,pr_faltas_recibidos=faltas_recibidas/minutos_jugados,pr_offsides=offsides/minutos_jugados,
                                                                   pr_amarillas=(amarillas+doble_amarilla)/minutos_jugados,pr_expulsados=(doble_amarilla+rojas)/minutos_jugados,pr_pase_correcto=pase_correcto/minutos_jugados,
                                                                   pr_incorrecto=pase_incorrecto/minutos_jugados,pr_despejes=despejes/minutos_jugados,pr_quites=quites/minutos_jugados,pr_atajadas=atajadas/minutos_jugados,jugador=paste(perso_apellido,perso_nombre,sep=","))

