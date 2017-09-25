library(xgboost)
library(dplyr)
library(pROC)

base_xgboost <- escalada_base[ind]
train_index <- caret::createDataPartition(escalada_base$resultado_local,p=0.7)

train_xgboost <- base_xgboost[train_index$Resample1,]
test_xgboost <- base_xgboost[-train_index$Resample1,]

xgb<-xgboost(data = as.matrix(train_xgboost), 
             label = as.matrix(train_completa$resultado_local), 
             eta = 0.01, 
             subsample = 0.7, 
             colsample_bytree = 0.4, 
             min_child_weight = 4, 
             max_depth = 6,
             alpha = 0, lambda = 0.1, gamma = 0.01,
             nround= 10000, 
             print_every_n = 100,
             nthread = 4,
             objective="binary:logistic"
)

pred_xgb <- predict(xgb, as.matrix(test_xgboost))
pred_xgb.corte <- ifelse(pred_xgb>0.45,1,0)
g <- roc(resultado_local ~ pred_xgb.corte, data = test_completa)
plot(g)
g

importantes_ambiente <- xgb.importance(colnames(train_xgboost),model =xgb )
cols <- importantes_ambiente %>% filter(Gain>=0.001) %>% select(Feature)

xgb_final<-xgboost(data = as.matrix(train_xgboost[,cols$Feature]), 
             label = as.matrix(train_completa$resultado_local), 
             eta = 0.01, 
             subsample = 0.7, 
             colsample_bytree = 0.4, 
             min_child_weight = 4, 
             max_depth = 6,
             alpha = 0, lambda = 0.1, gamma = 0.01,
             nround= 4000, 
             print_every_n = 100,
             nthread = 4,
             objective="binary:logistic"
)

pred_xgb_final <- predict(xgb_final, as.matrix(test_xgboost))
pred_xgb.corte_final <- ifelse(pred_xgb_final>0.45,1,0)
g2 <- roc(resultado_local ~ pred_xgb.corte_final, data = test_completa)
plot(g)
g

#------graficar 2 modelos----- # Sun Sep 24 21:47:23 2017 ------------------------------

plot(g)
lines(g2,col='red')
legend(0.2,0.4,c(paste("Ranger",round(g$auc,2)), paste("Xgboost",round(g2$auc,2))), 
       col = c('black','red'),lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5)) # gives the legend lines the correct color and width


