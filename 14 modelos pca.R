#----separar train y test----

base_modelado_completa$equipo_local <- factor(base_modelado_completa$equipo_local)
base_modelado_completa$equipo_visitante <- factor(base_modelado_completa$equipo_visitante)

base_modelado_completa$resultado_local <- factor(base_modelado_completa$resultado_local)


train_index <- caret::createDataPartition(base_modelado_completa$resultado_local,p=0.7)

train_completa <- base_modelado_completa[train_index$Resample1,]
test_completa <- base_modelado_completa[-train_index$Resample1,]

#----rpart test----


fit.rpart <- rpart(resultado_local ~ ., data = train_completa,control = rpart.control(minsplit = 5,minbucket = 2,cp=0.01,maxdepth = 20,split="information"))

rpart.plot(fit.rpart)

pred.rpart <- predict(fit.rpart,test_completa)

pred.rpart.corte <- ifelse(pred.rpart[,1]>0.6,0,1)

table(predicho=pred.rpart.corte,observador=test_completa$resultado_local)

g <- roc(resultado_local ~ pred.rpart.corte, data = test_completa)
plot(g, col="red")
g

#----ranger----

archivo_salida  <- "salida_ranger_roc_pca_v2.txt"

#escribo los  titulos  del archivo salida
if( !file.exists( archivo_salida) )
{
  cat( "fecha", "algoritmo", "corte_variable", "num.trees", "vmin.node.size", "AUC",  "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )
}

lineas_archivo <-  length( readLines(archivo_salida) )  - 1

for(  vnum.trees  in  c( 5, 10, 20, 50, 100, 200, 500, 800, 1000, 1500, 2000, 5000) )
{
  for(  vmin.node.size  in  c( 100, 50, 30, 20, 10, 8, 7, 6, 5, 3, 2, 1) )
  {
    ranger.fit   <- ranger(resultado_local ~ ., data = train_completa , num.trees=vnum.trees,  min.node.size=vmin.node.size, probability=TRUE )	
    
    ranger.pred  = predict( ranger.fit,  test_completa)
    for(c in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)) {
      pred.ranger.corte <- ifelse(ranger.pred$predictions[,2]>c,1,0)
      
      g <- roc(resultado_local ~ pred.ranger.corte, data = test_completa)
      
      cat(c ,  vnum.trees, vmin.node.size, g$auc)
      cat( format(Sys.time(), "%Y%m%d %H%M%S"), "ranger",c ,  vnum.trees, vmin.node.size, g$auc, "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )
    }
  }
  
  
}
