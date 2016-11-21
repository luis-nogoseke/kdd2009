require(xgboost)
require(doMC)
require(pROC)

set.seed(1234)
registerDoMC(5)

set.seed(1234)

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

dtrain <- xgb.DMatrix(data = data.matrix(train[,1:66]), label = data.matrix(train[,67]))
bstDMatrix <- xgboost(data = dtrain, max.depth = 15, eta = 0.1, nthread = 5, nround = 25, objective = "binary:logistic", eval_metric= 'auc')
pred <- predict(bstDMatrix, data.matrix(test[, 1:66]))


rocCurve   <- roc(response = test$appetency,
                      predictor = pred,
                      levels = rev(levels(test$appetency)))
plot(rocCurve, print.thres = "best")

pred.t <- pred
pred.t[pred <= 0.079 ] <- 1
pred.t[pred > 0.079 ] <- 2
pred.t <- as.factor(pred.t)
levels(pred.t) <- c("No", "Yes")
GetAUC(test$appetency, pred.t)


xgb <- xgboost(data = data.matrix(t3), 
 label = data.matrix(lab[,1]), 
 eta = 0.1,
 max_depth = 15, 
 nround=25,
 subsample = 0.5,
 colsample_bytree = 0.5,
 eval_metric = "merror",
 objective = "binary:logistic",
 num_class = 12,
 nthread = 3
)



labels <- subset(test, select = appetency)
n <- ncol(test)-1
test.xg <- test[,1:n]

factors <- sapply(test.xg, is.factor)
t2 <- data.frame(lapply(test.xg[, factors], function(x) model.matrix(~x-1,test.xg)))
t3 <- cbind(test[, !factors], t2)

p <- predict(xgb, newdata=data.matrix(t3))





# We need to seprate the data from the labels 
labels <- subset(train, select = appetency)
n <- ncol(train)-1
train.xg <- train[,1:n]


levels(labels$appetency) <- c("No", "Yes")
levels(test$appetency) <- c("No", "Yes")

factors <- sapply(train.xg, is.factor)
t2 <- data.frame(lapply(train.xg[, factors], function(x) model.matrix(~x-1,train.xg)))
t3 <- cbind(train[, !factors], t2)

lab <- model.matrix(~appetency-1,labels)