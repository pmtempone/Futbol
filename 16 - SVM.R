library(e1071)

svm_final <- svm(resultado_local ~ ., data=cbind(train_xgboost[,cols$Feature],resultado_local=train_completa$resultado_local),kernel="sigmoid")

summary(svm_final)

pred_svm <- predict(svm_final, test_xgboost[,cols$Feature], decision.values = TRUE)
attr(pred_svm, "decision.values")[1:2,]
