################################################################################
# libraries
library(caret)

# source files
source("lib/components.R")
source("lib/prepare.R")
################################################################################

################################################################################
# data specific

set.seed(123)
original.training <- read.csv("Ttrain.csv")
data.testing <- read.csv("Ttest.csv")
Passid <- data.testing$PassengerId
original.training$PassengerId <- NULL
original.training$Survived <- as.factor(original.training$Survived)
original.training$Pclass <- as.factor(original.training$Pclass)
levels(original.training$Survived) <- c("No", "Yes")
data.testing$Pclass <- as.factor(data.testing$Pclass)

formula <- Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Age

# Split training data for training models and for training the stacked model
in.training <- createDataPartition(original.training$Survived, p = .75,
                                   list = FALSE)
stack.training  <- original.training[-in.training, ]
data.training <- original.training[ in.training, ]

################################################################################

################################################################################
# Prepare

data.training <- TreatNumeric(data.training)
data.testing <- TreatNumeric(data.testing)
stack.training <- TreatNumeric(stack.training)
original.training <- TreatNumeric(original.training)
data.training <- TreatFactor(data.training)
data.testing <- TreatFactor(data.testing)
stack.training <- TreatFactor(stack.training)
original.training <- TreatFactor(original.training)
################################################################################

################################################################################
# Select Variables
################################################################################

################################################################################
# Train components

# 10-fold CV optimizing ROC
fit.control <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                           summaryFunction = twoClassSummary, classProbs = TRUE)

models <- CreateModels(data.training, formula, fit.control)

################################################################################

################################################################################
# Select
################################################################################

################################################################################
# Combine

predictions <- MakePredictions(models, stack.training)

predictions$target <- as.factor(stack.training$Survived)

glm1 <- glm(target ~ ., data = predictions, family = binomial,
            control = list(maxit = 50))
################################################################################

# ROC and AUC for the original training data
require(ROCR)
predictions <- MakePredictions(models, original.training)
pred <- predict(glm1, newdata = predictions, "response")
pr <- prediction(pred, original.training$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr") # ROC
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

################################################################################
# Testing
predictions <- MakePredictions(models, data.testing)
final <- predict(glm1, newdata = predictions, "response")
final <- ifelse(final > 0.5, 1, 0)
my_solution <- data.frame(PassengerId = Passid, Survived = final)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)


################################################################################
al <- c("gbm", "nnet", "C5.0", "rpart")
models <- caretList(formula, data = original.training, trControl = fit.control,
                    methodList = al)
results <- resamples(models)
summary(results)
dotplot(results)
modelCor(results)
splom(results)

stack.glm <- caretStack(models, method = "nnet", metric = "ROC",
                        trControl = fit.control)
print(stack.glm)
predict(stack.glm, newdata = data.testing)
final <- as.numeric(a)
final[final == 1] <- 0
final[final == 2] <- 1
