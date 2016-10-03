require(caret)
require(forcats)
source("lib/prepare.R")

set.seed(1234)

classes <- readRDS("smallclass.RDS")
t <- read.table("tcc-data/orange_small_train.data", nrow = 50000,
                sep = "\t", header = TRUE, colClasses = c(classes),
                stringsAsFactors = TRUE, na.strings = c(" ", "", "\t"))


target <- read.table("tcc-data/orange_small_train_churn.labels",
                     header = FALSE)
names(target) <- c("objective")
target$objective <- as.factor(target$objective)
target <- fct_recode(target$objective, A = "-1", B = "1")
t$objective <- target

################################################################
# Treat database
# Remove the target to allow the data preparation
temp_app <- t$objective
t$objective <- NULL

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

t$objective <-temp_app
rm(temp_app)
################################################################

# Split the data
in.training <- createDataPartition(t$objective, p = .75, list = FALSE)
testing <- t[-in.training, ]
t <- t[ in.training, ]

in.training <- createDataPartition(t$objective, p = .75, list = FALSE)
stack.training  <- t[-in.training, ]
t <- t[ in.training, ]

saveRDS(t, 'training_data.RDS')
saveRDS(testing, 'testing_data.RDS')
saveRDS(stack.training, 'stack_training.RDS')