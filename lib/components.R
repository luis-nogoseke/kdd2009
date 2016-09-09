require(caret)

CreateModels <- function(df, formula, control) {
  # Create classification models.
  #
  # Args:
  #   df: data frame with training data.
  #   formula: formula to train.
  #   control: control for caret models training
  #
  # Returns:
  #   The list of models trained

  # Stochastic Gradient Boosting
  gbm.fit1 <- train(formula, data = df, method = "gbm",
                   trControl = control, metric = "ROC", verbose = FALSE)
  # CART - Random Forest
  rpart1 <- train(formula, data = df, method = "rpart",
                  trControl = control, metric = "ROC")
  # Neural Network
  nnet1 <- train(formula, data = df, method = "nnet",
                 trControl = control, metric = "ROC", trace = FALSE)
  # C5.0
  C5 <- train(formula, data = df, method = "C5.0",
              trControl = control, metric = "ROC", verbose = FALSE)
  # TODO: A better way to create the list of models, maybe appending each one
  # as they are trained
  return (list("gbm" = gbm.fit1, "rpart" = rpart1, "nnet" = nnet1, "C5" = C5))
}

MakePredictions <- function(models, df) {
  # Makes prediction for the given data with the given list of models.
  #
  # Args:
  #   models: list of models.
  #   df: data frame with training data.
  #
  # Returns:
  #   Data frame with predictions.
  return (data.frame(lapply(models, predict, newdata = df)))
}
