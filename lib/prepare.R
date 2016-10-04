require(forcats)
require(caret)

Mode <- function(x) {
  # Find the mode.
  #
  # Args:
  #   x: vector.
  #
  # Returns:
  #   The mode of the vector
  ux <- unique(x)
  ux[which.max(tabulate(match(na.omit(x), ux)))]
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
  # as.factor(replace(x, is.na(x), Mode(x)))
  fct_explicit_na(x, Mode(x))
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

ReduceLevels <- function(df) {
  # Reduce the number of levels on factor columns to a maximum of 10 + "Other"
  #
  # Args:
  #   df: dataframe on which process.
  #
  # Returns:
  #   The dataframe with factor levels reduced.
  cols <- (sapply(df, function(x) nlevels(x)) > 10)
  if(sum(cols) == 1){
      df[, cols] <- fct_lump(df[, cols], n = 8)
      return (df)
  }
  df[, cols] <- lapply(df[, cols], function(x) fct_lump(x, n = 8))
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
  df[, factors] <- lapply(df[, factors], MissingFactor)
  return (df)
}

RemoveHighCorrelated <- function(df, th = 0.75) {
    # Remove the attributes that have high correlation.
    #
    # Args:
    #    df: dataframe to be analyzed.
    #    th: threshold to select high correlated atributes.
    #
    # Returns:
    #   The dataframe with the high correlated columns removed.

    numerics <- sapply(df, is.numeric)
    corr <- cor(df[, numerics])
    alta.corr <- findCorrelation(corr, cutoff = th)
    subset(df, select = -alta.corr)
}

RemoveHighMissing <- function(df, p = 0.6) {
    # Remove the attributes that have more than 60% of values missing
    #
    # Args:
    #    df: dataframe to be analyzed.
    #    p: threshold of the percentage of missing values.
    #
    # Returns:
    #   The dataframe with the selected columns removed.
    uniquelength <- sapply(df, function(x) length(unique(x)))
    df <- subset(df, select = uniquelength > 1)

    # Remove columns with more than 60% missing values
    na.count <-sapply(df, function(x) sum(length(which(is.na(x)))))
    df <- subset(df, select = na.count < (nrow(df) * p))
    return (df)
}
