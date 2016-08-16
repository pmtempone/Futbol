library(FactoMineR)
library(foreign)
library(psych)
library(knitr)
library(xtable)
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(dendextendRcpp))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggfortify))
library(corrplot)
#library(Stuff)
suppressPackageStartupMessages(library(dplyr))
library(broom)
library(plotly)

#a- Realizar un análisis de cluster jerárquico explicando la clasificación obtenida y el
#método elegido.

matjugadores <- as.matrix(jugadores_agr[,c(25:38)])
nombres <- iconv(jugadores_agr$jugador,to='ASCII//TRANSLIT') #por tildes en ubuntu
nombres <- jugadores_agr$jugador
rownames(matjugadores) <- nombres


d2 = dist(matjugadores,method = "euclidean")
jug.clust.sin = as.dendrogram(hclust(d2, method = "single")) %>% set("branches_lwd", 2)
jug.clust.com = as.dendrogram(hclust(d2, method = "complete")) %>% set("branches_lwd", 2)
jug.clust.avg = as.dendrogram(hclust(d2, method = "average")) %>% set("branches_lwd", 2)
jug.clust.ward = as.dendrogram(hclust(d2, method = "ward.D")) %>% set("branches_lwd", 2)
jug.dend = dendlist("Cercano" = jug.clust.sin, "Lejano" = jug.clust.com, "Promedio" = jug.clust.avg,"Ward"=jug.clust.ward)

t <- corrplot(cor.dendlist(jug.dend), "pie", "lower")
(ggt <- ggplotly(t))

plot(jug.clust.avg %>% set("branches_k_color", k=3) %>% set("branches_lwd", 2), main = "Average")
jug3 <- cutree(jug.clust.com,3)
jugadores_agr$clust <- factor(jug3)

Tabla <- describeBy(jugadores_agr[,c(25:38)], group = jugadores_agr$clust, mat = T)[,c("group1", "mean")]
Tabla <- cbind(Tabla,"promedio gral"=rep(colMeans(jugadores_agr[,c(25:38)]), each = 3))

Tabla

#b- Idem para un cluster no jerárquico.

pca.jug <- PCA(jugadores_agr[,c(25:38)])
PCA.1.2 = pca.jug$ind$coord[,c(1:2)]
PCA.1.3 = pca.jug$ind$coord[,c(1:3)]


kclusts <- data.frame(k=1:5) %>% group_by(k) %>% do(kclust=kmeans(jugadores_agr[,c(25:38)], .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))


assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], PCA.1.2))

clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

scrpl <- ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw()

(ggscrpl <- ggplotly(scrpl))

#4 grupos parece ser lo mejor por screeplot

k4 <-  ggplot(PCA.1.2, aes(x = Dim.1, y = Dim.2), theme = NULL) + geom_point(size = 3, color = subset(assignments, k == 4)$.cluster) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw() + ggtitle("Kmeans")
plot(k4)

(ggdim2 <- ggplotly(k4))

k4_dim3 = ggplot(PCA.1.3, aes(x = Dim.1, y = Dim.3), theme = NULL) + geom_point(size = 3, color = subset(assignments, k == 4)$.cluster) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw() + ggtitle("Kmeans")
plot(k4_dim3)

(ggdim3 <- ggplotly(k4_dim3))


subplot(ggdim2,ggdim3)


multiplot(k4,k4_dim3)

