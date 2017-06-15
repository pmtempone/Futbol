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

train_index <- caret::createDataPartition(base_modelado_completa$resultado_local,p=0.7)

train_completa <- base_modelado_completa[train_index$Resample1,]
test_completa <- base_modelado_completa[-train_index$Resample1,]


fit.rpart <- rpart(resultado_local ~ ., data = train_completa[,c(2:31,34)])

rpart.plot(fit.rpart)

pred.rpart <- predict(fit.rpart,test_completa)

pred.rpart.corte <- ifelse(pred.rpart>0.2,1,0)

table(predicho=pred.rpart.corte,observador=test_completa$resultado_local)

g <- roc(resultado_local ~ pred.rpart.corte, data = test_completa)
plot(g, col="red")
g
