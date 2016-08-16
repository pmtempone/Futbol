library(ggplot2)
library(FactoMineR)
library(dplyr)

DatosPremiumArgentina2013 <- read.csv("DatosPremiumArgentina2013.csv", sep=";")
DatosPremiumArgentina2014 <- read.csv("DatosPremiumArgentina2014.csv", sep=";")
DatosPremiumArgentina2015 <- read.csv("DatosPremiumArgentina2015.csv", sep=";")
DatosPremiumArgentina2016 <- read.csv("DatosPremiumArgentina2016.csv", sep=";")

DatosPremiumArgentina2013[,c("local","visitante","perso_nombre","perso_apellido","team")] <- NULL
DatosPremiumArgentina2014[,c("local","visitante","perso_nombre","perso_apellido","team")] <- NULL
DatosPremiumArgentina2015[,c("local","visitante","perso_nombre","perso_apellido","team")] <- NULL
DatosPremiumArgentina2016[,c("local","visitante","perso_nombre","perso_apellido","team")] <- NULL


DatosPremiumArgentina2016$rol_id_rol <- factor(DatosPremiumArgentina2016$rol_id_rol)

summary(DatosPremiumArgentina2016)

DatosPremiumArgentina2016$local.1 <- as.character(DatosPremiumArgentina2016$local.1)

DatosPremiumArgentina2016$team.1 <- as.character(DatosPremiumArgentina2016$team.1)


DatosPremiumArgentina2016$J_local <- ifelse(DatosPremiumArgentina2016$team.1==DatosPremiumArgentina2016$local.1,"L","V")

ggplot(data = DatosPremiumArgentina2016, aes(x = J_local,y=disparo_afuera, fill = J_local)) + geom_boxplot()
ggplot(data = DatosPremiumArgentina2016, aes(x = J_local,y=pase_correcto, fill = J_local)) + geom_boxplot()


datos2016 <- DatosPremiumArgentina2016[DatosPremiumArgentina2016$team.1!='Argentina',]
