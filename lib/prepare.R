Mode <- function(x) {
  # Find the mode.
  #
  # Args:
  #   x: vector.
  #
  # Returns:
  #   The mode of the vector
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

MissingNumeric <- function(x) {
  # Replace mising value on vector with the mean.
  #
  # Args:
  #   x: vector.
  #
  # Returns:
  #   The vector with missing values replaced by the mean.
  replace(x, is.na(x), mean(x, na.rm = TRUE))
}

MissingFactor <- function(x) {
  # Replace missing factor value with mode.
  #
  # Args:
  #   x: factor vector.
  #
  # Returns:
  #   The vector with missing values replaced by the mode.
  replace(x, is.na(x), Mode(x))
}

TreatNumeric <- function(df) {
  # Preprocess numeric data on a dataframe.
  #
  # Args:
  #   df: dataframe on which to preproccess numeric data.
  #
  # Returns:
  #   The dataframe with numeric atributes preprocessed.

  # Get numeric columns
  numerics <- sapply(df, is.numeric)
  # Replace missing values
  df[, numerics] <- apply(df[, numerics], 2, MissingNumeric)
  # Scale values
  df[, numerics] <- scale(df[, numerics])
  return (df)
}

TreatFactor <- function(df) {
  # Preprocess factor data on a dataframe.
  #
  # Args:
  #   df: dataframe on which to preproccess factor data.
  #
  # Returns:
  #   The dataframe with factor atributes preprocessed.

  # Get factor columns
  factors <- sapply(df, is.factor)
  # Replace missing values
  df[, factors] <- apply(df[, factors], 2, MissingFactor)
  return (df)
}
