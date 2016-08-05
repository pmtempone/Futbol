library(ggplot2)
library(FactoMineR)
library(dplyr)

DatosPremiumArgentina2013 <- read.csv("C:/Users/Pablo/Dropbox/Futbol/DatosPremiumArgentina2013.csv", sep=";")
DatosPremiumArgentina2014 <- read.csv("C:/Users/Pablo/Dropbox/Futbol/DatosPremiumArgentina2014.csv", sep=";")
DatosPremiumArgentina2015 <- read.csv("C:/Users/Pablo/Dropbox/Futbol/DatosPremiumArgentina2015.csv", sep=";")
DatosPremiumArgentina2016 <- read.csv("C:/Users/Pablo/Dropbox/Futbol/DatosPremiumArgentina2016.csv", sep=";")

DatosPremiumArgentina2016$rol_id_rol <- factor(DatosPremiumArgentina2016$rol_id_rol)

summary(DatosPremiumArgentina2016)

DatosPremiumArgentina2016$local <- as.character(DatosPremiumArgentina2016$local)

DatosPremiumArgentina2016$team <- as.character(DatosPremiumArgentina2016$team)


DatosPremiumArgentina2016$J_local <- ifelse(DatosPremiumArgentina2016$team==DatosPremiumArgentina2016$local,"L","V")

