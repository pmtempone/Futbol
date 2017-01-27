----#librerias----
library(xtable)
library(sqldf)
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
library(readxl)   #Para leer los archivos de excel
library(psych)    #Para calcular las estadísticas por grupo
library(nortest)  #Para hacer las pruebas de hipótesis de normalidad
library(lawstat)  #Prueba de Levene
library(reshape)  #Manejo de datos
library(foreign)
library(dplyr)

library(Rcmdr)
options(scipen = 999)

----#crear dataset con tiros de titulares y suplentes----
  
jugadores_tiros =  jugadores_var %>% dplyr::select(jugador,J_local,pr_disparo_afuera)

jugadores_tiros <- acast(jugadores_tiros, jugador~J_local, value.var="pr_disparo_afuera")

jugadores_tiros <- as.data.frame(jugadores_tiros)
jugadores_tiros <- jugadores_tiros[complete.cases(jugadores_tiros),]


----#graficos de densidad----

ggplot() + geom_density(aes(x=L), colour="red", data=jugadores_tiros) + 
  geom_density(aes(x=V), colour="blue", data=jugadores_tiros)

jugadores_grafico <- jugadores_var %>% dplyr::select(jugador,J_local,pr_disparo_afuera)

lista <- as.data.frame(rownames(jugadores_tiros))
jugadores_grafico <- sqldf("select * from jugadores_grafico where jugador in (select * from lista)")

ggplot(jugadores_var %>% dplyr::select(jugador,J_local,pr_disparo_afuera),aes(x=pr_disparo_afuera, fill=J_local)) + geom_density(alpha=0.25)

ggplot(data=jugadores_grafico,aes(x=J_local,y=pr_disparo_afuera,fill=J_local)) + geom_boxplot()


----#Hipotesis-----

'Hipótesis nula:
Los promedios de las tiros al arco de afuera de los jugadores son iguales tanto de visitante como de local.
H0:μ1=μ2=⋯=μn=μ
Hipótesis alternativa:
Existe al menos un promedio de las  tiros al arco de afuera de los jugadores tanto de visitante como de local es diferente de los demás.
Ha:∃μi≠μ
, i=1,…,n
Análisis discriminante:'

describeBy(jugadores_grafico$pr_disparo_afuera, group = jugadores_grafico$J_local, mat = T)[,c("group1", "mean", "sd")]

jugadores.AOV = aov(data = jugadores_grafico %>% filter(jugadores_grafico$pr_disparo_afuera>0), pr_disparo_afuera ~ J_local)
jugadores.AOV
summary(jugadores.AOV)
latex(summary(jugadores.AOV), title="Anova",file = "")            # If you want all the data
print(xtable(summary(jugadores.AOV)))
#b- Realice la prueba y el análisis diagnostico(supuestos)
jugadores_dif_medias = cbind.data.frame(jugadores_grafico %>% filter(jugadores_grafico$pr_disparo_afuera>0), residuals(jugadores.AOV))
colnames(jugadores_dif_medias)[4] = "Anova.Residuales"


Anderson <- ad.test(jugadores_dif_medias$Anova.Residuales)
Kolmogorov <- lillie.test(jugadores_dif_medias$Anova.Residuales)
ShapiroWilk <- shapiro.test(jugadores_dif_medias$Anova.Residuales)
Cramer <- cvm.test(jugadores_dif_medias$Anova.Residuales)

print(xtable(rbind(cbind(Anderson$method,'p valor' = Anderson$p.value),
      cbind(Kolmogorov$method,'p valor' = Kolmogorov$p.value),
      cbind(ShapiroWilk$method,'p valor' = ShapiroWilk$p.value),
      cbind(Cramer$method,'p valor' = Cramer$p.value))
))
#Todas las pruebas de normalidad rechaza la hipótesis nula, es decir, no se puede asumir que los residuales del modelo tienen distribución normal.

#Pruebas de homocedasticidad:

Levene <- leveneTest(jugadores_dif_medias$Anova.Residuales, jugadores_dif_medias$J_local)
Bartlett <- bartlett.test(jugadores_dif_medias$Anova.Residuales, jugadores_dif_medias$J_local)

print(xtable(rbind(cbind("Levene",'p valor' = Levene$`Pr(>F)`),
      cbind("Bartlett",'p valor' = Bartlett$p.value))
))
#No Se satisface la hipótesis de homocedasticidad entre los grupos.


TukeyHSD(jugadores.AOV)


##se realiza transformacion de boxcox ya que no se cumplen supuestos

boxcox(pr_disparo_afuera ~ 1,data = jugadores_grafico %>% filter(jugadores_grafico$pr_disparo_afuera>0),plotit=T)



----#pruebas no parametricas para dos poblaciones identicas----

#al no poder asumir la normalidad hacemos el test para dos poblaciones relacionadas

#wilcoxon signed rank test

wilc.jugadores <- wilcox.test(jugadores_tiros$L,jugadores_tiros$V, paired = TRUE)

ggplot(data = jugadores_var[jugadores_var$pr_goles_convertidos<0.08 & jugadores_var$pr_disparo_afuera<0.15,],aes(x=pr_disparo_afuera,y=pr_goles_convertidos,colour=J_local))+geom_point()
