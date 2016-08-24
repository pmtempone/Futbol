----#carga de librerias----

library(dplyr)
library(FactoMineR)
library(ggplot2)

----#Analisis de componentes principales con jugadores----

pca_jugadores <- PCA(matjugadores,scale.unit=TRUE, ncp=5, graph=T)
ellipse.coord = coord.ellipse(concat,bary=T)
plot.PCA(pca_jugadores, axes=c(1, 2), choix="ind")

dimdesc(pca_jugadores, axes=c(1,2))


----#HCLUST-----

res.hcpc = HCPC(pca_jugadores)

res.hcpc

res.hcpc$desc.var$quanti.var

----#cantidad de PC-----

FactoMineR::estim_ncp(matjugadores) #3 componentes


FactoMineR::plot.HCPC(res.hcpc)
