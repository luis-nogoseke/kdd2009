require(caret)
require(pROC)
require(ROCR)

source("lib/prepare.R")

# To guarantee reproducible results
set.seed(1234)

classes <- readRDS("smallclass.RDS")
t <- read.table("tcc-data/orange_small_train.data", nrow = 50000,
                sep = "\t", header = TRUE, colClasses = c(classes),
                stringsAsFactors = TRUE, na.strings = c(" ", "", "\t"))

# Remove columns that have only the same value
uniquelength <- sapply(t, function(x) length(unique(x)))
t <- subset(t, select = uniquelength > 1)

# Remove columns with more than 60% missing values
na.count <-sapply(t, function(x) sum(length(which(is.na(x)))))
t <- subset(t, select = na.count < 30000)

# Treat the missing values and scale the numeric ones
t <- TreatNumeric(t)
t <- TreatFactor(t)

# Remove atributes with correlation higher than 75%
t <- RemoveHighCorrelated(t)


# Add the target
target <- read.table("tcc-data/orange_small_train_appetency.labels",
                     header = FALSE)
names(target) <- c("appetency")
target$appetency <- as.factor(target$appetency)
target <- fct_recode(target$appetency, A = "-1", B = "1")
t$appetency <- target

# Train models
fit.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                           summaryFunction = twoClassSummary, classProbs = TRUE,
                           verboseIter = TRUE)

formula <- appetency ~ .

model <- glm(formula,family=binomial(link='logit'),data=t)

gbm.fit1 <- train(formula, data = t, method = "gbm", trControl = fit.control,
                  metric = "ROC", verbose = FALSE)

# svm.fit1 <- train(formula, data = t, method = "svmRadial", trControl = fit.control,
#                   metric = "ROC")
#
#
# nnet1 <- train(formula, data = t, method = "nnet", trControl = fit.control,
#                metric = "ROC", trace = FALSE)
