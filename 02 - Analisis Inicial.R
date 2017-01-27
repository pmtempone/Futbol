----#carga de librerias#-----

library(ggplot2)
library(dplyr)
library(plyr)
library(reshape)
library(GGally)
library(lubridate)
library(SportsAnalytics)
library(archetypes)
library("RColorBrewer")
library(lattice)

-----#Analisis de datos#-----

summary(datos2016)

head(datos2016[datos2016$rol_id_rol=='5',]) # tecnicos rol 5

datos2016 <- datos2016[datos2016$rol_id_rol!='5',]

-----#transformacion datos#-----

dat.m <- melt(datos2016,id.vars='J_local', measure.vars=c("goles_convertidos","goles_encontra","asistencias","disparo_afuera","disparo_palo","disparo_atajado","penal_errado","faltas","offsides","amarillas","doble_amarilla","rojas","pase_correcto","pase_incorrecto","despejes","quites","atajadas","atajada_penal"))


-------#multboxplot#--------
p <- ggplot(dat.m) + geom_boxplot(aes(x=J_local, y=value, color=variable))

p+facet_wrap( ~ variable, scales="free")

(gg <- ggplotly(p))

-----#correlaciones pases#-------
ggpairs(datos2016,mapping = ggplot2::aes(color=J_local),columns = c("goles_convertidos","asistencias","disparo_afuera","disparo_atajado","pase_correcto","despejes","quites"))

-----#grafico de barras local vs visitante de rol id y titular#----
t1 <- count(datos2016,vars = c("J_local","rol_id_rol"))

p1 <- ggplot(t1,aes(x=rol_id_rol,y=freq,fill=J_local))+geom_bar(stat="identity",position="dodge")

t2 <- count(datos2016,vars = c("J_local","titular"))

p2 <- ggplot(t2,aes(x=titular,y=freq,fill=J_local))+geom_bar(stat="identity",position="dodge")

(ggp2 <- ggplotly(p2))

multiplot(p1,p2)
#head(as.Date(datos2016$fecha,"%d/%m/%Y"))
datos2016$fecha <- as.Date(datos2016$fecha,"%d/%m/%Y")

l1 <- ggplot(datos2016,aes(x=fecha,y=goles_convertidos))+geom_line(stat="identity",position="dodge")

#head(month(datos2016$fecha)) funcion de lubridate

l1 <- ggplot(datos2016,aes(x=month(fecha),y=goles_convertidos))+ geom_point() +
  xlab("") + ylab("goles convertidos")
l1

----#datos por jugador#------

jugadores <- datos2016 %>% select(perso_nombre.1,perso_apellido.1,rol_id_rol,team.1,minutos_jugados:atajada_penal)%>%group_by(perso_nombre.1,perso_apellido.1,rol_id_rol,team.1)

jugadores_agr <- jugadores %>%summarise_each(funs(sum)) %>% mutate(pr_goles_convertidos=goles_convertidos/minutos_jugados,pr_asistencias=asistencias/minutos_jugados,
                                                                   pr_disparo_afuera=disparo_afuera/minutos_jugados,pr_disparo_atajado=disparo_atajado/minutos_jugados,
                                                                   pr_faltas=faltas/minutos_jugados,pr_faltas_recibidos=faltas_recibidas/minutos_jugados,pr_offsides=offsides/minutos_jugados,
                                                                   pr_amarillas=(amarillas+doble_amarilla)/minutos_jugados,pr_expulsados=(doble_amarilla+rojas)/minutos_jugados,pr_pase_correcto=pase_correcto/minutos_jugados,
                                                                   pr_incorrecto=pase_incorrecto/minutos_jugados,pr_despejes=despejes/minutos_jugados,pr_quites=quites/minutos_jugados,pr_atajadas=atajadas/minutos_jugados,jugador=paste(perso_apellido.1,perso_nombre.1,sep=","))


----#archetypes of players------
col_pal <- brewer.pal(7, "Set1")

col_black <- rgb(0, 0, 0, 0.2)
#mat <- as.matrix(subset(dat, select = -c(Team, Name, Number, Nationality,WeakFootAccuracy, WeakFootFrequency)))

rownames(matjugadores) <- NULL

pcplot(matjugadores, col = col_black, las = 2)

set.seed(1234)

as <- stepArchetypes(matjugadores, k = 1:14)

screeplot(as)

a4 <- bestModel(as[[5]])

parameters(a4)

barplot(a4, matjugadores, percentiles = TRUE)

pcplot(a4, matjugadores, data.col = col_black, atypes.col = col_pal[1:5])

legend("topleft", legend = sprintf("A%s", 1:5),col = col_pal[1:5], lwd = 1, bg = "white")

--------### Alpha coefficients:------

coef <- coef(a4, "alphas")

pcplot(coef, col = c(NA, NA, col_black),rx = matrix(c(0, 1), ncol = 5, nrow = 2), var.label = FALSE)


coef <- coef(a4, "alphas")


## The best player is a combination of Archetyp 1 and Archetype 2 with
## Archetype 1 contributing more than Archetype 2:
which <- which(coef[, 3] == 0 & coef[, 4] == 0 &
                 coef[, 1] > 0 & coef[, 2] > 0 &
                 coef[, 1] < coef[, 2])

cbind(jugadores_agr[which, c("jugador")],
      coef[which, ])

## ... in relation to player position:
 pos <- as.character(jugadores$rol_id_rol)

cols <- rep("gray", length(pos))

cols[pos == "Defender"] <- col_pal[1]


pcplot(coef, col = c(NA, NA, cols),
       rx = matrix(c(0, 1), ncol = 4, nrow = 2), var.label = FALSE)

#Good players:
 
good_players <- function(atype, threshold) {
     which <- which(coef(a4, "alphas")[, atype] > threshold)
     good_coef <- coef(a4, "alphas")[which, ]
      good_dat <- subset(jugadores_agr[which, ], select = c(jugador))
      good_dat <- cbind(good_dat, good_coef)
      good_dat <- good_dat[order(-good_coef[, atype]), ]
      good_dat
      }

good_threshold <- 0.95

players <- lapply(1:5, good_players, good_threshold)

players


myColours <- brewer.pal(6,"Blues")
my.settings <- list(
  superpose.polygon=list(col=myColours[2:5], border="transparent"),
  strip.background=list(col=myColours[6]),
  strip.border=list(col="black")
)

parallelplot(matjugadores,horizontal.axis=FALSE, par.settings=my.settings)


ggparcoord(matjugadores, columns = c(1:3),scale = "centerObs")+geom_line()
