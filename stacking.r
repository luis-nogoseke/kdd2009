require(rpart)
require(foreach)
require(doMC)
require(nnet)
require(caret)
require(ROCR)
require(pROC)

set.seed(1234)
registerDoMC(5)

full <- readRDS('prepdata/full.RD')


levels(full$churn) <- c('No', 'Yes')
levels(full$appetency) <- c('No', 'Yes')
levels(full$upselling) <- c('No', 'Yes')
labels <- subset(full, select = appetency:upselling)
n <- ncol(full)-3
full <- full[,1:n]


full$appetency <- labels$appetency

in.training <- createDataPartition(full$appetency, p = .75, list = FALSE)
train.stack <-  full[-in.training, ]
train <- full[ in.training, ]

in.training <- createDataPartition(train$appetency, p = .75, list = FALSE)
test <- train[-in.training, ]
train <- train[ in.training, ]

formula <- appetency ~ .

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

#####################################################

bagging <- function(formula, training, length_divisor=3, iterations=10) {
    models<-foreach(m=1:iterations, .packages='rpart') %dopar% {
        training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)), replace=TRUE)
        train_pos<-1:nrow(training) %in% training_positions
        model <- rpart(formula, data=training[train_pos,], method='class', control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
        model
    }
    return (models)
}


MakePredictions <- function(model, newdata) {
    predictions <- foreach(a=iter(model), .combine=cbind.data.frame, .packages='rpart') %dopar% {
        p = predict(a, newdata, 'prob');
        p
    }
    return (predictions)
}

Veto <- function(x) {
    ux <- unique(x)
    if(length(ux) > 1)
        return (1)
    else
        return (ux[[1]])
}

#########################################

m <- bagging(formula, train, iterations=100)

p1 <- MakePredictions(m, train.stack)

pt <- MakePredictions(m, test)

fit.control <- trainControl(method = "repeatedcv",
                            number = 2,
                            repeats = 1,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            savePredictions = "final")

final_blender_model <- train(p1, train.stack[, 'appetency'], method='nnet', metric='ROC', trControl=fit.control)

preds <- predict(object=final_blender_model, pt)
GetAUC(test$appetency, preds)

preds <- predict(object=final_blender_model, pt, 'prob')
rocCurve   <- roc(response = test$appetency,
                      predictor = preds[, "Yes"],
                      levels = rev(levels(test$appetency)))
plot(rocCurve, print.thres = "best")


#########################################


fit.control <- trainControl(method = "repeatedcv",
                            number = 2,
                            repeats = 1,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            savePredictions = "final")

model_gbm <- train(train[,1:58], train[,59], method='gbm', metric='ROC', trControl=fit.control, distribution = "bernoulli")
model_rpart <- train(train[,1:58], train[,59], method='rpart', metric='ROC', trControl=fit.control)
model_treebag <- train(train[,1:58], train[,59], method='treebag', metric='ROC', trControl=fit.control)
model_nnet <- train(train[,1:58], train[,59], method='nnet', metric='ROC', trControl=fit.control)

model_xg <- train(train[,1:58], train[,59], method='xgbTree', metric='ROC', trControl=fit.control)

#Var126
#Var218  parecem importantes

gbmImp <- varImp(model_gbm, scale = FALSE)

preds.stack <- as.data.frame(as.data.frame(predict(object=model_gbm, train.stack[,1:58], 'prob'))$Yes)
names(preds.stack) <- 'gbm_PROB'
preds.stack$rf_PROB <- as.data.frame(predict(object=model_rpart, train.stack[,1:58], 'prob'))$Yes
preds.stack$treebag_PROB <- as.data.frame(predict(object=model_treebag, train.stack[,1:58], 'prob'))$Yes
preds.stack$nnet_PROB <- as.data.frame(predict(object=model_nnet, train.stack[,1:58], 'prob'))$Yes
preds.stack$appetency <- train.stack$appetency
test.stack <- as.data.frame(as.data.frame(predict(object=model_gbm, test[,1:58], 'prob'))$Yes)
names(test.stack) <- 'gbm_PROB'
test.stack$rf_PROB <- as.data.frame(predict(object=model_rpart, test[,1:58], 'prob'))$Yes
test.stack$treebag_PROB <- as.data.frame(predict(object=model_treebag, test[,1:58], 'prob'))$Yes
test.stack$nnet_PROB <- as.data.frame(predict(object=model_nnet, test[,1:58], 'prob'))$Yes
test.stack$appetency <- test$appetency

predictors <- names(preds.stack)[names(preds.stack) != 'appetency']
final_blender_model <- train(preds.stack[, predictors], preds.stack[, 'appetency'], method='nnet', metric='ROC', trControl=fit.control)

# train.stack$gbm_PROB <- predict(object=model_gbm, train.stack[,predictors])
# train.stack$rf_PROB <- predict(object=model_rpart, train.stack[,predictors])
# train.stack$treebag_PROB <- predict(object=model_treebag, train.stack[,predictors])
# test$gbm_PROB <- predict(object=model_gbm, test[,1:58])
# test$rf_PROB <- predict(object=model_rpart, test[,1:58])
# test$treebag_PROB <- predict(object=model_treebag, test[,1:58])
# predictors <- names(train.stack)[names(train.stack) != 'appetency']
# final_blender_model <- train(train.stack[,predictors], train.stack[, 'appetency'], method='nnet', trControl=fit.control)




preds <- predict(object=final_blender_model, test.stack[,predictors])
GetAUC(test$appetency, preds)


preds <- predict(object=final_blender_model, test.stack[,predictors], 'prob')
rocCurve   <- roc(response = test$appetency,
                      predictor = preds[, "Yes"],
                      levels = rev(levels(test$appetency)))
plot(rocCurve, print.thres = "best")


preds <- predict(object=final_blender_model, test.stack[,predictors], 'prob')
plot(preds$Yes, test$appetency)
pred.t <- preds$Yes
pred.t[preds$Yes <= 0.015 ] <- 1
pred.t[preds$Yes > 0.015 ] <- 2
pred.t <- as.factor(pred.t)
levels(pred.t) <- c("No", "Yes")
GetAUC(test$appetency, pred.t)
# appetency 0.016
# [1] 0.7243613
# 
#     pred.t
#        No  Yes
#  No  6999 2209
#  Yes   52  115



# churn 0.65
# [1] 0.6740876
# 
#     pred.t
#        No  Yes
#  No  5423 3263
#  Yes  190  498

# upselling 0.09
# [1] 0.7480964
# 
#     pred.t
#        No  Yes
#  No  7556 1128
#  Yes  258  432



