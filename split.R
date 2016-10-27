require(caret)
require(forcats)

set.seed(1234)

classes <- readRDS("smallclass.RDS")
t <- read.table("tcc-data/orange_small_train.data", nrow = 50000,
                sep = "\t", header = TRUE, colClasses = c(classes),
                stringsAsFactors = TRUE, na.strings = c(" ", "", "\t"))


target <- read.table("tcc-data/orange_small_train_appetency.labels",
                       header = FALSE)
target <- cbind(target, read.table("tcc-data/orange_small_train_churn.labels",
                       header = FALSE))
target <- cbind(target, read.table("tcc-data/orange_small_train_upselling.labels",
                        header = FALSE))

names(target) <- c("appetency", "churn", "upselling")
target$appetency <- as.factor(target$appetency)
target$churn <- as.factor(target$churn)
target$upselling <- as.factor(target$upselling)

target$appetency <- fct_recode(target$appetency, "0" = "-1")
target$churn <- fct_recode(target$churn, "0" = "-1")
target$upselling <- fct_recode(target$upselling, "0" = "-1")

t$appetency <- target$appetency
t$churn <- target$churn
t$upselling <- target$upselling

# Split the data
in.training <- createDataPartition(t$appetency, p = .75, list = FALSE)
testing <- t[-in.training, ]
t <- t[ in.training, ]

in.training <- createDataPartition(t$appetency, p = .75, list = FALSE)
stack.training  <- t[-in.training, ]
t <- t[ in.training, ]

saveRDS(t, 'training_data.RDS')
saveRDS(testing, 'testing_data.RDS')
saveRDS(stack.training, 'stack_training.RDS')
