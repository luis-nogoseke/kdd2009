original.classes <- readRDS("originalClasses.RD")
training <- read.table("tcc-data/training.data", sep = "\t", header = TRUE,
                       colClasses = c(original.classes), nrow = 50000)

# Remove columns with the same value
uniquelength <- sapply(training, function(x) length(unique(x)))
training <- subset(training, select = uniquelength > 1)

# Append the churn target
target <- read.table("tcc-data/orange_large_train_churn.labels", header = FALSE)
names(target) <- "churn"
training$churn <- target$churn

# Save in binary format
saveRDS(training, file = "rawData.RD")
