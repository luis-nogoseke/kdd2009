require(foreach)
require(doMC)
require(class)
require(nnet)
require(caret)
require(randomForest)
library(caretEnsemble)

set.seed(1234)
registerDoMC(4)
train <- readRDS('prepdata/train.RD')
test <- readRDS('prepdata/test.RD')


GetAUC <- function(real, predicted) {
    mc <- table(real, predicted)
    Vp <- mc[2,2]
    Fp <- mc[2,1]
    Fn <- mc[1,2]
    Vn <- mc[1,1]
    Sen <- Vp/(Vp+Fp)
    Esp <- Vn/(Vn+Fn)
    
    AUC <- (Esp+Sen)/2
    return (AUC)
}

formula <- appetency ~ .

levels(train$appetency) <- c("No", "Yes")
levels(test$appetency) <- c("No", "Yes")

fit.control <- trainControl(method = "repeatedcv", number = 2, repeats = 1,
                            summaryFunction = twoClassSummary, classProbs = TRUE, index=createResample(train$appetency, 25), savePredictions = "final")

model_list <- caretList(
  x=train[, 1:66], y=train[,67],
  trControl=fit.control,
  methodList=c("rpart", "nnet")
  )

greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
    ))
summary(greedy_ensemble)


model_preds <- lapply(model_list, predict, newdata=test, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"Yes"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=test, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, test$appetency)

pred.t <- model_preds$ensemble
pred.t[model_preds$ensemble <= 0.014 ] <- 1
pred.t[model_preds$ensemble > 0.014 ] <- 2
pred.t <- as.factor(pred.t)
levels(pred.t) <- c("No", "Yes")
GetAUC(test$appetency, pred.t)
# churn [1] 0.7202333

glm_ensemble <- caretStack(
  model_list,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

model_preds2 <- model_preds
model_preds2$ensemble <- predict(glm_ensemble, newdata=test, type="prob")
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds2, test$appetency)


pred.t <- model_preds2$ensemble
pred.t[model_preds2$ensemble <= 0.014 ] <- 1
pred.t[model_preds2$ensemble > 0.014 ] <- 2
pred.t <- as.factor(pred.t)
levels(pred.t) <- c("No", "Yes")
GetAUC(test$appetency, pred.t)







model_list <- caretList(
  x=train[, 1:66], y=train[,67],
  trControl=fit.control,
  methodList=c("rpart", "nnet", "gbm"),
  tuneList=list(
      nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
    )
  )

greedy_ensemble <- caretEnsemble(
  model_list, 
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
    ))
summary(greedy_ensemble)


model_preds <- lapply(model_list, predict, newdata=test, type="prob")
model_preds <- lapply(model_preds, function(x) x[,"Yes"])
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=test, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, test$appetency)

pred.t <- model_preds$ensemble
pred.t[model_preds$ensemble <= 0.014 ] <- 1
pred.t[model_preds$ensemble > 0.014 ] <- 2
pred.t <- as.factor(pred.t)
levels(pred.t) <- c("No", "Yes")
GetAUC(test$appetency, pred.t)
# churn [1] 0.7202333

glm_ensemble <- caretStack(
  model_list,
  method="glm",
  metric="ROC",
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    classProbs=TRUE,
    summaryFunction=twoClassSummary
  )
)

model_preds2 <- model_preds
model_preds2$ensemble <- predict(glm_ensemble, newdata=test, type="prob")
CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
colAUC(model_preds2, test$appetency)


pred.t <- model_preds2$ensemble
pred.t[model_preds2$ensemble <= 0.014 ] <- 1
pred.t[model_preds2$ensemble > 0.014 ] <- 2
pred.t <- as.factor(pred.t)
levels(pred.t) <- c("No", "Yes")
GetAUC(test$appetency, pred.t)
