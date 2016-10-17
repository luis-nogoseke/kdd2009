require(rpart)
require(foreach)
require(doMC)

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

base.tree <- rpart(formula, data=train, method='class', control=rpart.control(minsplit=2, minbucket=1, cp=0.001))

pred <- predict(base.tree, newdata=test, 'class')


# 0.5243514
base.AUC <- GetAUC(test$appetency, pred)
#####################################################

bagging <- function(formula, training, length_divisor=3, iterations=10) {
    models<-foreach(m=1:iterations, .packages='rpart') %dopar% {
        training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
        train_pos<-1:nrow(training) %in% training_positions
        model <- rpart(formula, data=training[train_pos,], method='class', control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
        model
    }
    return (models)
}


m <- bagging(formula, train)

MakePredictions <- function(model, newdata) {
    predictions <- foreach(a=iter(model), .combine=cbind.data.frame, .packages='rpart') %dopar% {
        p = predict(a, newdata, 'class');
        p
    }
    return (predictions)
}

p1 <- MakePredictions(m, test)


aucs <- foreach(a=iter(p1), .combine=c) %dopar% {
    GetAUC(test$appetency, a)
}
# [1] 0.5084557 0.5196232 0.5188126 0.5469922 0.5081906 0.5308069 0.5349041
# [8] 0.5308722 0.5271984 0.5144141

Majority <- function(x) {
    tabulatedOutcomes <- table(x)
    sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
    mostCommonLabel <- names(sortedOutcomes)[1]
    mostCommonLabel
}

p.majority <- as.factor(apply(p1, 1, Majority))
GetAUC(test$appetency, p.majority)
# 0.5043416

Veto <- function(x) {
    ux <- unique(x)
    if(length(ux) > 1)
        return (1)
    else
        return (ux[[1]])
}

p.veto <- as.factor(apply(p1, 1, Veto))
GetAUC(test$appetency, p.veto)
# 0.6318631