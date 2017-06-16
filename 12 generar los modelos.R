#modelo inicial

library(rpart)
library(rpart.plot)
library(party)
library(caret)
library(rattle)
library(pROC)
library(rpart.utils)
library(randomForest)
library(funModeling)
library(dplyr)
library(foreach)
library(arules)
library(ranger)

#----generar base de modelado-----

base_modelado_locales <- base_locales[,1:2] %>% left_join(avg_equipos_eventos,by=c("even_id_evento"="even_id_evento","equipo_local"="equipo")) %>%
  left_join(sum_equipos_eventos,by=c("even_id_evento"="even_id_evento","equipo_local"="equipo")) %>% left_join(max_equipos_eventos,by=c("even_id_evento"="even_id_evento","equipo_local"="equipo")) %>% left_join(min_equipos_eventos,by=c("even_id_evento"="even_id_evento","equipo_local"="equipo"))%>% left_join(fct_resultados_ult_partidos,by=c("even_id_evento"="even_id_evento","equipo_local"="equipo"))
base_modelado_visitantes <- base_visitantes[,1:2] %>% left_join(avg_equipos_eventos,by=c("even_id_evento"="even_id_evento","equipo_visitante"="equipo")) %>%
  left_join(sum_equipos_eventos,by=c("even_id_evento"="even_id_evento","equipo_visitante"="equipo")) %>% left_join(max_equipos_eventos,by=c("even_id_evento"="even_id_evento","equipo_visitante"="equipo")) %>% left_join(min_equipos_eventos,by=c("even_id_evento"="even_id_evento","equipo_visitante"="equipo"))%>% left_join(fct_resultados_ult_partidos,by=c("even_id_evento"="even_id_evento","equipo_visitante"="equipo"))

base_modelado_completa <- base_modelado_locales %>% left_join(base_modelado_visitantes,by="even_id_evento")

base_modelado_completa <- base_modelado_completa %>% left_join(lkp_eventos[,c("even_id_evento","fixt_local_goles","fixt_visitante_goles")],by="even_id_evento")

base_modelado_completa <- base_modelado_completa %>% mutate(resultado_local=ifelse(fixt_local_goles>fixt_visitante_goles,1,0))

base_modelado_completa <- base_modelado_completa[complete.cases(base_modelado_completa),]
#----separar train y test----

base_modelado_completa$equipo_local <- factor(base_modelado_completa$equipo_local)
base_modelado_completa$equipo_visitante <- factor(base_modelado_completa$equipo_visitante)

base_modelado_completa$resultado_local <- factor(base_modelado_completa$resultado_local)


train_index <- caret::createDataPartition(base_modelado_completa$resultado_local,p=0.7)

train_completa <- base_modelado_completa[train_index$Resample1,]
test_completa <- base_modelado_completa[-train_index$Resample1,]


fit.rpart <- rpart(resultado_local ~ ., data = train_completa[,c(2:121,124)],control = rpart.control(minsplit = 5,minbucket = 2,cp=0.01,maxdepth = 20,split="gini"))

rpart.plot(fit.rpart)

pred.rpart <- predict(fit.rpart,test_completa)

pred.rpart.corte <- ifelse(pred.rpart>0.33,1,0)

table(predicho=pred.rpart.corte,observador=test_completa$resultado_local)

g <- roc(resultado_local ~ pred.rpart.corte, data = test_completa)
plot(g, col="red")
g

#----ranger----

archivo_salida  <- "salida_ranger_roc.txt"

#escribo los  titulos  del archivo salida
if( !file.exists( archivo_salida) )
{
  cat( "fecha", "algoritmo", "corte_variable", "num.trees", "vmin.node.size", "AUC",  "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )
}

lineas_archivo <-  length( readLines(archivo_salida) )  - 1

for(  vnum.trees  in  c( 5, 10, 20, 50, 100, 200, 500, 800, 1000, 1500, 2000, 5000) )
{
  for(  vmin.node.size  in  c( 10000, 5000, 3000, 2000, 1000, 800, 700, 600, 500, 300, 200, 100, 50, 20) )
  {
    ranger.fit   <- ranger(resultado_local ~ ., data = train_completa[,c(2:121,124)] , num.trees=vnum.trees,  min.node.size=vmin.node.size, probability=TRUE )	

    ranger.pred  = predict( ranger.fit,  test_completa)
    for(c in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)) {
    pred.ranger.corte <- ifelse(ranger.pred$predictions[,2]>c,1,0)
    
    g <- roc(resultado_local ~ pred.ranger.corte, data = test_completa)
    
    cat(c ,  vnum.trees, vmin.node.size, g$auc)
    cat( format(Sys.time(), "%Y%m%d %H%M%S"), "ranger",c ,  vnum.trees, vmin.node.size, g$auc, "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )
    }
  }
  
  
}


