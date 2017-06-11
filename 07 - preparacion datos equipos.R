----##librerias-----

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
library(FactoMineR)
library(factoextra)
library("aplpack")

options(scipen = 999)

#----#preparacion de set a nivel equipos----

#---#agrupar por equipo---
  
  #Idea_1: sumar valores y ponderarlos por 90 min. Que es la duracion de un partido.
  
#----#preparacion de datos----

equipos_tot <- Basetotal %>%  dplyr::select(fecha,team.1,minutos_jugados:atajada_penal) %>% group_by(fecha,team.1)

equipos_tot <- equipos_tot %>%summarise_each(funs(sum,n_distinct(fecha,team.1)))%>%dplyr::select(fecha:minutos_jugados_n_distinct)

equipos_tot <- equipos_tot %>% mutate(partidos=minutos_jugados_n_distinct)

equipos_tot$minutos_jugados_n_distinct <- NULL

equipos_tot_df <- as.data.frame(equipos_tot)

equipos_tot_df <- equipos_tot_df %>% dplyr::select(-fecha)%>% group_by(team.1) %>%summarise_each(funs(sum))

equipos_tot_df <- equipos_tot_df %>%  mutate(pr_goles_convertidos=goles_convertidos_sum/partidos,pr_asistencias=asistencias_sum/partidos,
                                             pr_disparo_afuera=disparo_afuera_sum/partidos,pr_disparo_atajado=disparo_atajado_sum/partidos,
                                             pr_faltas=faltas_sum/partidos,pr_faltas_recibidos=faltas_recibidas_sum/partidos,pr_offsides=offsides_sum/partidos,
                                             pr_amarillas=(amarillas_sum+doble_amarilla_sum)/partidos,pr_expulsados=(doble_amarilla_sum+rojas_sum)/partidos,pr_pase_correcto=pase_correcto_sum/partidos,
                                             pr_incorrecto=pase_incorrecto_sum/partidos,pr_despejes=despejes_sum/partidos,pr_quites=quites_sum/partidos,pr_atajadas=atajadas_sum/partidos)

#----#4.1 pca equipos-----

matequipos <- as.matrix(equipos_tot_df[,c(22:35)])
rownames(matequipos) <- equipos_tot_df$team.1


parallelplot(matequipos,horizontal.axis=FALSE, scales = list(x = list(rot = 90)),col=col_black)


pca.equipos <- PCA(equipos_tot_df[,c(22:35)])
PCA.equipos.1.2 = pca.equipos$ind$coord[,c(1:2)]
PCA.equipos.1.3 = pca.equipos$ind$coord[,c(1:3)]
eigenvalues_equipos <- pca.equipos$eig

head(eigenvalues_equipos)


pca.equipos <- PCA(matequipos,scale.unit=TRUE, ncp=5, graph=T,axes = c(1,2))

fviz_screeplot(pca.equipos, ncp=14)

fviz_pca_var(pca.equipos, col.var="contrib") ##variables PCA con el color por contribucion

fviz_pca_ind(pca.equipos,  label="none", habillage=res.hcpc$data.clust$clust,addEllipses=TRUE, ellipse.level=0.95) #jugadores por cluster

#----#4.2 clusters equipos-----

#----#HCLUST con 2 grupos-----

resequipos.hcpc = HCPC(pca.equipos)

resequipos.hcpc

resequipos.hcpc$desc.var$quanti.var

fviz_pca_ind(pca.equipos,  label="none", habillage=resequipos.hcpc$data.clust$clust,addEllipses=TRUE, ellipse.level=0.95) #jugadores por cluster


fviz_pca_biplot(pca.equipos, 
                habillage = resequipos.hcpc$data.clust$clust, addEllipses = TRUE,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()


#-----#4.3 caras de chernoff-------



caras <- faces(equipos_tot_df[,c(22:35)],face.type=1,labels=equipos_tot_df$team.1)

print(xtable(caras$info))

