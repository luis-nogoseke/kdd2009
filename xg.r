require(xgboost)
require(doMC)
require(pROC)
require(caret)

set.seed(1234)
registerDoMC(5)


full <- readRDS('prepdata/full.RD')


labels <- subset(full, select = appetency:upselling)
n <- ncol(full)-3
full <- full[,1:n]


full$churn <- labels$churn

in.training <- createDataPartition(full$churn, p = .75, list = FALSE)
train.stack <-  full[-in.training, ]
train <- full[ in.training, ]

in.training <- createDataPartition(train$churn, p = .75, list = FALSE)
test <- train[-in.training, ]
train <- train[ in.training, ]

#in.training <- createDataPartition(full$churn, p = .75, list = FALSE)
#test <- full[-in.training, ]
#train <- full[ in.training, ]


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


dtrain <- xgb.DMatrix(data = data.matrix(train[,1:58]), label = data.matrix(train[,59]))
bstDMatrix <- xgboost(data = dtrain,
                     # nfold=10,
                      max.depth = 10,
                      eta = 0.05,
                      subsample=0.5,
                      max_delta_step=1,
                      scale_pos_weight=0.5,
                      min_child_weight = 1,
                      colsample_bytree=0.5,
                      nround = 150,
                      objective = "binary:logistic",
                      eval_metric= 'auc',
                      nthread=5)
pred <- predict(bstDMatrix, data.matrix(test[, 1:58]))


rocCurve   <- roc(response = test$churn,
                      predictor = pred,
                      levels = rev(levels(test$churn)))
# plot(rocCurve, print.thres = "best")
rocCurve
th <- coords(rocCurve, 'best', ret='threshold')

pred.t <- pred
pred.t[pred <= th ] <- 1
pred.t[pred > th ] <- 2
pred.t <- as.factor(pred.t)
levels(pred.t) <- c("No", "Yes")
GetAUC(test$churn, pred.t)



# model_xg <- train(data.matrix(train[,1:58]), data.matrix(l), method='xgbTree', metric='ROC', trControl=fit.control)
##############################################
# appetency
# bstDMatrix <- xgboost(data = dtrain, max.depth = 10, eta = 0.02, nthread = 5, nround = 80, objective = "binary:logistic", eval_metric= 'auc', nthread=5)
# 0.01223349   0.7411009
#      No  Yes
#  0 7197 2011
#  1   50  117
#
# upselling
# 0.07776084  0.762449
#      No  Yes
#  0 7176 1508
#  1  208  482
#
# churn
# 0.0478997 0.6625029
#
#      No  Yes
#  0 6762 1924
#  1  312  376
