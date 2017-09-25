library(e1071)

svm_final <- svm(resultado_local ~ ., data=cbind(train_xgboost[,cols$Feature],resultado_local=train_completa$resultado_local),kernel="sigmoid")

summary(svm_final)

pred_svm <- predict(svm_final, test_xgboost[,cols$Feature], decision.values = TRUE)
attr(pred_svm, "decision.values")[1:2,]


tunning_svm <- tune(svm,resultado_local ~.,data=cbind(train_xgboost[,cols$Feature],resultado_local=train_completa$resultado_local), gamma = 2^(-1:1), cost = 2^(2:4))
summary(tunning_svm)
plot(tunning_svm)
g3 <- roc(resultado_local ~ as.integer(as.character(pred_svm)), data = test_completa,gamma = 2^(-1:1), cost = 2^(2:4))
plot(g3)
g3
