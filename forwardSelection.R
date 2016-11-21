require(rpart)
require(foreach)
require(doMC)
require(caret)

set.seed(1234)
registerDoMC(4)
full <- readRDS('prepdata/full.RD')



in.training <- createDataPartition(full$appetency, p = .75, list = FALSE)
selection <- full[-in.training, ]
train <- full[ in.training, ]

in.training <- createDataPartition(train$appetency, p = .75, list = FALSE)
stacking <- train[-in.training, ]
train <- train[ in.training, ]

formula <- appetency ~ .

Veto <- function(x) {
    ux <- unique(x)
    if(length(ux) > 1)
        return (1)
    else
        return (ux[[1]])
}

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




bagging <- function(formula, training, length_divisor=3, iterations=10) {
    models<-foreach(m=1:iterations, .packages='rpart') %dopar% {
        training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)), replace=TRUE)
        train_pos<-1:nrow(training) %in% training_positions
        model <- rpart(formula, data=training[train_pos,], method='class', control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
        model
    }
    return (models)
}

MakePredictions <- function(models, newdata) {
    predictions <- foreach(a=iter(models), .combine=cbind.data.frame, .packages='rpart') %dopar% {
        p = predict(a, newdata, 'class');
        p
    }
    return (predictions)
}



# Train the models
m <- bagging(formula, train, iterations=100)

# Create an empty ensemble
ensemble <- list()

# make initial predictions
p1 <- MakePredictions(m, selection)
aucs <- foreach(a=iter(p1), .combine=c) %dopar% {
    GetAUC(selection$appetency, a)
}

# Put the best model onto the ensemble
index <- which(aucs==max(aucs))
ensemble[1] <- m[index] 
aux <- max(aucs)

# Forward selection with replacement
max.size <- 29
pb <- txtProgressBar(min = 0, max = max.size, style = 3)
for(i in 1:max.size) {
    setTxtProgressBar(pb, i)
    aucs <- foreach(a=iter(m), .combine=c, .packages='rpart') %dopar% {
        temp <- ensemble
        temp[[i+1]] <- a
        pred <- MakePredictions(temp, selection)
        pred.final <- as.factor(apply(pred, 1, Veto))
        GetAUC(selection$appetency, pred.final)
    }
    if(max(aucs) - aux < 0.001){
        break
    }
    aux <- max(aucs)
    index <- which(aucs==aux)
    ensemble[[i+1]] <- m[index] 
}
close(pb)

# 10 selected from 50 0.9454335
# using all 50 models 0.8500611
pred <- MakePredictions(ensemble, stacking)
pred.final <- as.factor(apply(pred, 1, Veto))
GetAUC(stacking$appetency, pred.final)


pred <- MakePredictions(m, stacking)
pred.final <- as.factor(apply(pred, 1, Veto))
GetAUC(stacking$appetency, pred.final)

