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
library(fmsb)

options(scipen = 999)


#----#star plot de jugadores----

#subset jugadores videla, acuña, aued, gastón diaz, marcelo meli,pulpito gonzalez,vismara, cerro

Jugadores_bonus <- jugadores_agr %>% filter(iconv(jugador,'latin1') %in% c("Meli,Cesar Marcelo","Videla,Ezequiel","Acuña,Marcos","Diaz,Ricardo Gaston","Aued,Luciano Roman","Gonzalez,Diego Hernan","Cerro,Francisco"))

Jugadores_bonus <- Jugadores_bonus[,c(23:37)]

#-----#Meli---------

#MAX y min filas

max_radar <- data.frame(max(Jugadores_bonus[,1]),max(Jugadores_bonus[,2]),max(Jugadores_bonus[,3]),max(Jugadores_bonus[,4]),max(Jugadores_bonus[,5]),
                        max(Jugadores_bonus[,6]),max(Jugadores_bonus[,7]),max(Jugadores_bonus[,8]),max(Jugadores_bonus[,9]),max(Jugadores_bonus[,10]),
                        max(Jugadores_bonus[,11]),max(Jugadores_bonus[,12]),max(Jugadores_bonus[,13])) 


colnames(max_radar) <- colnames(Jugadores_bonus[,1:13])

Meli=rbind(max_radar, rep(0,13) , Jugadores_bonus[1,1:13])


# Custom the radarChart !
melichart <- radarchart( Meli  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8, title=Jugadores_bonus$jugador[1]
)


#-----#Gonzalez--------

Gonzalez=rbind(max_radar, rep(0,13) , Jugadores_bonus[2,1:13])


# Custom the radarChart !
Gonzalez_chart <- radarchart( Gonzalez  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8, title=Jugadores_bonus$jugador[2]
)

#-----#Videla--------

Videla=rbind(max_radar, rep(0,13) , Jugadores_bonus[3,1:13])


# Custom the radarChart !
Videla_chart <- radarchart( Videla  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8, title=Jugadores_bonus$jugador[3]
)

#-----#Cerro--------

Cerro=rbind(max_radar, rep(0,13) , Jugadores_bonus[4,1:13])


# Custom the radarChart !
Cerro_chart <- radarchart( Cerro  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8, title=Jugadores_bonus$jugador[4]
)


#-----#Aued--------

Aued=rbind(max_radar, rep(0,13) , Jugadores_bonus[5,1:13])


# Custom the radarChart !
Aued_chart <- radarchart( Aued  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8, title=Jugadores_bonus$jugador[5]
)


#-----#Acuña--------

Acuña=rbind(max_radar, rep(0,13) , Jugadores_bonus[6,1:13])


# Custom the radarChart !
Acuna_chart <- radarchart( Acuña  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8, title=Jugadores_bonus$jugador[6]
)

#-----#Diaz--------

Diaz=rbind(max_radar, rep(0,13) , Jugadores_bonus[7,1:13])


# Custom the radarChart !
Diaz_chart <- radarchart( Diaz  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8, title=Jugadores_bonus$jugador[7]
)

#-----#todos los graficos juntos----

par(mfrow=c(4,2)) 
par(mfrow=c(1,1)) 

#---#chernoff jugadores----

faces(Jugadores_bonus[,c(1:13)],face.type=1,labels=Jugadores_bonus$jugador)
