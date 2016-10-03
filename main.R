require(caret)
require(pROC)
require(ROCR)

source("lib/prepare.R")
source("lib/myRandomForrest.R")
last <- function(x) { tail(x, n = 1) }
# To guarantee reproducible results
set.seed(314159265)


t <- readRDS('training_data.RDS')
testing <- readRDS('testing_data.RDS')
stack.training <- readRDS('stack_training.RDS')

# # Train models

#library(randomForest)
#tree <- randomForest(upselling ~ ., data = t, ntree = 50)pedict

####################################################
# Train models
trainForest(t)

####################################################

#prediction <- predict(tree, t, type = "prob")
#solution <- data.frame(upselling = t$upselling, predicted = prediction)
#truth_table = table(solution$upselling, solution$predicted)
#truth_table

p <- predict(tree, newdata=t)
truth_table = confusionMatrix(t$upselling, p)
truth_table

Vp <- truth_table$table[1,1]
Fp <- truth_table$table[1,2]
Fn <- truth_table$table[2,1]
Vn <- truth_table$table[2,2]

Sen <- Vp/(Vp+Fp)
Esp <- Vn/(Vn+Fn)
AUC <- (Esp+Sen)/2
AUC

##TESTING
p2 <- predict(tree, newdata=testing)
truth_table = confusionMatrix(testing$upselling, p2)
truth_table

Vp <- truth_table$table[1,1]
Fp <- truth_table$table[1,2]
Fn <- truth_table$table[2,1]
Vn <- truth_table$table[2,2]

Sen <- Vp/(Vp+Fp)
Esp <- Vn/(Vn+Fn)
AUC <- (Esp+Sen)/2
AUC

prediction <- predict(tree, testing, type = "prob")
plot(performance(prediction(prediction[,2], testing$upselling), 'tpr', 'fpr'))
performance(prediction(prediction[,2], testing$upselling), measure= "auc")
#table(temp_app, p)
