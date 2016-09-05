----#carga de librerias-----

library(psych)    #Para calcular las estadísticas por grupo
library(ggplot2)  #Para graficar
library(nortest)  #Para hacer las pruebas de hipótesis de normalidad
library(lawstat)  #Prueba de Levene
library(reshape)  #Manejo de datos
library(foreign)
library(dplyr)
library(reshape2)
library(MASS)
library(Rcmdr)
library(multcomp)
library("mvtnorm")
library(ICSNP) #para hotteling
library(Hotelling)
suppressPackageStartupMessages(library(psych))
library(MVN)


----#preparacion de datos----

jugadores_Local <- Basetotal %>% mutate(jugador=paste(perso_apellido.1,perso_nombre.1,sep=","))%>% dplyr::select(jugador,J_local,minutos_jugados:atajada_penal) %>% group_by(jugador,J_local)

jugadores_Local <- jugadores_Local %>%summarise_each(funs(sum)) 

quantile(jugadores_Local$minutos_jugados,0.05) #ver la poblacion con menores minutos disputados

jugadores_var <- jugadores_Local %>% filter(minutos_jugados>=90)

jugadores_var <- jugadores_Local %>% mutate(pr_goles_convertidos=goles_convertidos/minutos_jugados,pr_asistencias=asistencias/minutos_jugados,
                                          pr_disparo_afuera=disparo_afuera/minutos_jugados,pr_disparo_atajado=disparo_atajado/minutos_jugados,
                                          pr_faltas=faltas/minutos_jugados,pr_faltas_recibidos=faltas_recibidas/minutos_jugados,pr_offsides=offsides/minutos_jugados,
                                          pr_amarillas=(amarillas+doble_amarilla)/minutos_jugados,pr_expulsados=(doble_amarilla+rojas)/minutos_jugados,pr_pase_correcto=pase_correcto/minutos_jugados,
                                          pr_incorrecto=pase_incorrecto/minutos_jugados,pr_despejes=despejes/minutos_jugados,pr_quites=quites/minutos_jugados,pr_atajadas=atajadas/minutos_jugados)



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


#No se satisface el supuesto de normalidad multivariada, por lo tanto no es posible hacer un análisis discriminante a menos que se encuentre una transformación que permita normalizar los datos.



roystonTest(Y[, -16:-15])
