#----#carga de librerias----

library(dplyr)
library(FactoMineR)
library(ggplot2)
library(factoextra)
library(ggthemes)

#----#Analisis de componentes principales con jugadores----

pca_jugadores <- PCA(matjugadores,scale.unit=TRUE, ncp=5, graph=T,axes = c(1,2))
#ellipse.coord = coord.ellipse(concat,bary=T)
plot.PCA(pca_jugadores, axes=c(1, 2), choix="ind",title = "Jugadores 1era División")

dimdesc(pca_jugadores, axes=c(1,3))


#----#HCLUST-----

res.hcpc = HCPC(pca_jugadores)

res.hcpc

res.hcpc$desc.var$quanti.var

#----#cantidad de PC-----

FactoMineR::estim_ncp(matjugadores) #3 componentes


FactoMineR::plot.HCPC(res.hcpc)

#----#graficar screeplot----

eigenvalues_jugadores <- pca_jugadores$eig

head(eigenvalues_jugadores)
options(encoding = "UTF-8")

barplot(eigenvalues_jugadores[, 2], names.arg=1:nrow(eigenvalues_jugadores), 
        main = "",
        xlab = "Componentes Principales",
        ylab = "Porcentaje de varianza explicada",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eigenvalues_jugadores), eigenvalues_jugadores[, 2], 
      type="b", pch=19, col = "red")

fviz_screeplot(pca_jugadores, ncp=14,title='')+theme_tufte()+xlab("Dimensiones")+ylab('Porcentaje de variación explicada')

fviz_pca_var(pca_jugadores, col.var="contrib",title="")+theme_tufte() ##variables PCA con el color por contribucion

fviz_pca_ind(pca_jugadores,  label="none",title="", habillage=res.hcpc$data.clust$clust,addEllipses=TRUE, ellipse.level=0.95,legend.title='Grupos') +theme_tufte() #jugadores por cluster


fviz_pca_biplot(pca_jugadores, title = '',legend.title='Grupos',
                habillage = res.hcpc$data.clust$clust, addEllipses = TRUE,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_tufte()
