require(caret)
require(forcats)

set.seed(1234)

classes <- readRDS("smallclass.RDS")
t <- read.table("tcc-data/orange_small_train.data", nrow = 50000,
                sep = "\t", header = TRUE, colClasses = c(classes),
                stringsAsFactors = TRUE, na.strings = c(" ", "", "\t"))


target <- read.table("tcc-data/orange_small_train_appetency.labels",
                     header = FALSE)
names(target) <- c("appetency")
target$appetency <- as.factor(target$appetency)
target <- fct_recode(target$appetency, "0" = "-1")
t$appetency <- target

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
