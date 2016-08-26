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

----#preparacion de datos----

jugadores_Local <- Basetotal %>% dplyr::select(perso_nombre.1,perso_apellido.1,J_local,minutos_jugados:atajada_penal) %>% group_by(perso_nombre.1,perso_apellido.1,J_local)

jugadores_Local <- jugadores_Local %>%summarise_each(funs(sum)) 

quantile(jugadores_Local$minutos_jugados,0.05) #ver la poblacion con menores minutos disputados

jugadores_var <- jugadores_Local %>% filter(minutos_jugados>=90)

jugadores_var <- jugadores_Local %>% mutate(pr_goles_convertidos=goles_convertidos/minutos_jugados,pr_asistencias=asistencias/minutos_jugados,
                                          pr_disparo_afuera=disparo_afuera/minutos_jugados,pr_disparo_atajado=disparo_atajado/minutos_jugados,
                                          pr_faltas=faltas/minutos_jugados,pr_faltas_recibidos=faltas_recibidas/minutos_jugados,pr_offsides=offsides/minutos_jugados,
                                          pr_amarillas=(amarillas+doble_amarilla)/minutos_jugados,pr_expulsados=(doble_amarilla+rojas)/minutos_jugados,pr_pase_correcto=pase_correcto/minutos_jugados,
                                          pr_incorrecto=pase_incorrecto/minutos_jugados,pr_despejes=despejes/minutos_jugados,pr_quites=quites/minutos_jugados,pr_atajadas=atajadas/minutos_jugados,jugador=paste(perso_apellido.1,perso_nombre.1,sep=","))



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

describeBy(Y[,2:15], group = Y$J_local, mat = T)[,c("group1", "mean", "sd")]
Y$J_local <- factor(Y$J_local)

#melt para hacer multiples boxplots

Y_v2 <- Y %>% 
  ungroup(c(perso_apellido.1,perso_nombre.1))

Y_v2 <- as.data.frame(Y_v2)

df.m <- melt(Y_v2,id.vars = "J_local")

ggplot(data = df.m, aes(x = variable, y = value)) + geom_boxplot(aes(fill=J_local))

----#test de hotelling-----
(m1 <- with(Y, HotellingsT2(cbind(pr_goles_convertidos,pr_asistencias,pr_disparo_afuera,
                                  pr_disparo_atajado,pr_faltas,pr_faltas_recibidos,pr_offsides,
                                  pr_amarillas,pr_expulsados,pr_pase_correcto,pr_incorrecto,
                                  pr_despejes,pr_quites,pr_atajadas) ~ J_local)))

#se rechaza la hipotesis nula, no se comportan de igual manera los jugadores de local y visitante

y_mat <- as.matrix(Y)

fit = hotelling.test(.~J_local, Y, perm = TRUE)
fit
plot(fit)
plot(fit, col = "lightblue")



data(bottle.df)
bottle.df = subset(bottle.df, Number == 1)
bottle.df$Number = rep(1:2,c(10,10))
bottle.df$Number = factor(bottle.df$Number)
fit = hotelling.test(.~Number, bottle.df, perm = TRUE)
plot(fit)
plot(fit, col = "lightblue")
jugadores.MAOV =  manova(data = Y, J_local~ .)
ratas.AOV
summary(ratas.AOV)
#se rechaza supuesto de igualdad de medias

#b- Realice la prueba y el análisis diagnostico(supuestos)
ratas = cbind(ratas, residuals(ratas.AOV))
colnames(ratas)[3] = "Anova.Residuales"

#se prueba normalidad

Anderson <- ad.test(ratas$Anova.Residuales)
Kolmogorov <- lillie.test(ratas$Anova.Residuales)
ShapiroWilk <- shapiro.test(ratas$Anova.Residuales)
Cramer <- cvm.test(ratas$Anova.Residuales)

rbind(cbind(Anderson$method,'p valor' = Anderson$p.value),
      cbind(Kolmogorov$method,'p valor' = Kolmogorov$p.value),
      cbind(ShapiroWilk$method,'p valor' = ShapiroWilk$p.value),
      cbind(Cramer$method,'p valor' = Cramer$p.value))

#Ninguna de las pruebas de normalidad rechaza la hipótesis nula, es decir, se puede asumir que los residuales del modelo tienen distribución normal.

#Pruebas de homocedasticidad:

Levene <- leveneTest(ratas$Anova.Residuales, ratas$tratamiento)
Bartlett <- bartlett.test(ratas$Anova.Residuales, ratas$tratamiento)

rbind(cbind("Levene",'p valor' = Levene$`Pr(>F)`),
      cbind("Bartlett",'p valor' = Bartlett$p.value))

#Se satisface la hipótesis de homocedasticidad entre los grupos.

#c- Si es válida, concluya, si no lo es, utilice otra prueba y concluya.

'Se puede afirmar que hay diferencias significativas entre los grupos dado que se satisfacen los supuestos.'

#d- Explique en qué casos realizaría transformaciones de las variables.

#Se hacen transformaciones de las variables cuando no se satisfacen los supuestos de normalidad y/o homocedasticidad. Para ello, se suele hacer un gráfico de medias vs desviaciones estandar para determinar el tipo de transformación a usar (Caso de homocedasticidad).
boxcox(ratas$globulos~1,plotit=T) #como se haria