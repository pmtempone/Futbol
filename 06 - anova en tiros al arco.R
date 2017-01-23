----#librerias----

library(corrplot)
library(scatterplot3d)
library(rgl)
library(foreign)
library(MASS)
library(usdm) #evaluar vif
library(CombMSC)
library(ggplot2)
library(caret)
library(car)
library(ellipse)
library(QuantPsyc)
library(leaps)
library(xlsx)
library(dummies)
library(ggcorrplot)
library(fmsb)
library(reshape2)
options(scipen = 999)

----#crear dataset con tiros de titulares y suplentes---
  
jugadores_tiros <- jugadores_var %>% dplyr::select(jugador,J_local,pr_disparo_afuera)

jugadores_tiros <- acast(jugadores_tiros, jugador~J_local, value.var="pr_disparo_afuera")

jugadores_tiros <- as.data.frame(jugadores_tiros)
jugadores_tiros <- jugadores_tiros[complete.cases(jugadores_tiros),]

----#graficos de densidad----

ggplot() + geom_density(aes(x=L), colour="red", data=jugadores_tiros) + 
  geom_density(aes(x=V), colour="blue", data=jugadores_tiros)



ggplot(jugadores_var %>% dplyr::select(jugador,J_local,pr_disparo_afuera),aes(x=pr_disparo_afuera, fill=J_local)) + geom_density(alpha=0.25)


----#Hipotesis-----

'Hipótesis nula:
Los promedios de las actuaciones de los jugadores son iguales tanto de visitante como de local.
H0:μ1=μ2=⋯=μn=μ
Hipótesis alternativa:
Existe al menos un promedio de las actuaciones de los jugadores tanto de visitante como de local es diferente de los demás.
Ha:∃μi≠μ
, i=1,…,n
Análisis discriminante:'

Y <- jugadores_var %>% dplyr::select(J_local,pr_goles_convertidos:pr_atajadas)

Y$perso_nombre.1 <- NULL
Y$perso_apellido.1 <- NULL

Y$J_local <- factor(Y$J_local)

describeBy(Y[,2:15], group = Y$J_local, mat = T)[,c("group1", "mean", "sd")]

#melt para hacer multiples boxplots

Y_v2 <- as.data.frame(jugadores_var[,c(2,22:35)])

df.m <- melt(Y_v2,id.vars = "J_local")

ggplot(data = df.m, aes(x = variable, y = value)) + geom_boxplot(aes(fill=J_local))

----#test de hotelling-----
(m1 <- with(Y_v2, HotellingsT2(cbind(pr_goles_convertidos,pr_asistencias,pr_disparo_afuera,
                                     pr_disparo_atajado,pr_faltas,pr_faltas_recibidos,pr_offsides,
                                     pr_amarillas,pr_expulsados,pr_pase_correcto,pr_incorrecto,
                                     pr_despejes,pr_quites,pr_atajadas) ~ J_local)))

#se rechaza la hipotesis nula, no se comportan de igual manera los jugadores de local y visitante

y_mat <- as.matrix(Y)

fit = hotelling.test(.~J_local, Y, perm = TRUE)
fit
plot(fit)
plot(fit, col = "lightblue")

#Normalidad multivariada (Se usa el test de Royston):'

roystonTest(Y_v2[, -1])

m2 <- manova(cbind(pr_goles_convertidos,pr_asistencias,pr_disparo_afuera,
                   pr_disparo_atajado,pr_faltas,pr_faltas_recibidos,pr_offsides,
                   pr_amarillas,pr_expulsados,pr_pase_correcto,pr_incorrecto,
                   pr_despejes,pr_quites,pr_atajadas) ~ J_local,Y_v2)

summary(m2) #no son iguales las varianzas

#No se satisface el supuesto de normalidad multivariada, por lo tanto no es posible hacer un análisis discriminante a menos que se encuentre una transformación que permita normalizar los datos.
