----#carga de librerias----

library(dplyr)
library(FactoMineR)
library(ggplot2)

----#Analisis de componentes principales con jugadores----

pca_jugadores <- PCA(matjugadores,scale.unit=TRUE, ncp=5, graph=T)

plot.PCA(pca_jugadores, axes=c(1, 2), choix="ind", habillage=2)
