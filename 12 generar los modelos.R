#modelo inicial

library(rpart)
library(rpart.plot)


fit.rpart <- rpart(resultado_local ~ ., data = base_modelado_completa[,c(2:31,34)])

rpart.plot(fit.rpart)
