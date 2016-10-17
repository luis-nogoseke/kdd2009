require(caret)
require(pROC)
require(ROCR)
require(randomForest)

source("lib/prepare.R")
last <- function(x) { tail(x, n = 1) }
# To guarantee reproducible results
set.seed(1234)

t <- readRDS('training_data.RDS')

# Remove the target to allow the data preparation
temp_app <- t$appetency
t$appetency <- NULL

# Remove columns that have only the same value
uniquelength <- sapply(t, function(x) length(unique(x)))
t <- subset(t, select = uniquelength > 1)

# Remove columns with more than 60% missing values
na.count <-sapply(t, function(x) sum(length(which(is.na(x)))))
t <- subset(t, select = na.count < 17000)


# Treat the missing values and scale the numeric ones
t <- TreatNumeric(t)
t <- TreatFactor(t)
t <- ReduceLevels(t)
# Remove atributes with correlation higher than 75%
t <- RemoveHighCorrelated(t)

# re-append the target

t$appetency <-temp_app
rm(temp_app)

# Train models
fit.control <- trainControl(method = "repeatedcv", number = 10, repeats = 1,
                           summaryFunction = twoClassSummary, classProbs = TRUE,
                           verboseIter = TRUE)

formula <- appetency ~ .

#model <- glm(formula,family=binomial(link='logit'),data=t)

# ada <- train(formula, data = t, method = "ada", trControl = fit.control,
#              metric = "ROC", verbose = TRUE)
# saveRDS(ada,'ada.RD')


rf <- randomForest(formula, t, importance=TRUE, ntree=300)
# saveRDS(rf,'rf.RD')

#gbm.fit1 <- train(formula, data = t, method = "gbm", trControl = fit.control,
#                  metric = "ROC", verbose = TRUE)


#rf.fit <- train(formula, data = t, method = "rf", trControl = fit.control,
#                metric = "ROC", verbose = TRUE)
# svm.fit1 <- train(formula, data = t, method = "svmRadial", trControl = fit.control,
#                   metric = "ROC")
#
#
# nnet1 <- train(formula, data = t, method = "nnet", trControl = fit.control,
#                metric = "ROC", trace = FALSE)

attributes <- names(t)
testing<-readRDS('testing_data.RDS')[, attributes]
attributes <- attributes[-length(attributes)] # Remove the target
# saveRDS(attributes, 'adaAttributes.RD')
temp_app <- testing$appetency
testing$appetency <- NULL

testing <- TreatNumeric(testing)
testing <- TreatFactor(testing)


# Adapt the factors levels on the testing data according to what was selected on
# the training data
facs <- sapply(t, is.factor)
b <- sapply(t[,facs], levels)
uniquelength <- sapply(b, function(x) length(unique(x)))
b <- subset(b, uniquelength > 8)
# saveRDS(b, 'adaFacLevels.RD')
for (n in names(b)){
      testing[,n] <- fct_collapse(testing[,n], Other = subset(levels(testing[,n]), !(levels(testing[,n]) %in% b[[n]])))
}

p <- predict(rf, newdata=testing, "prob")
plot(performance(prediction(p[,2], temp_app), 'tpr', 'fpr'))
auc<-performance(prediction(p[,2], temp_app), measure= "auc")
print(auc@y.values[[1]])

p2 <- predict(rf, newdata=testing)
confusionMatrix(temp_app, p2)
