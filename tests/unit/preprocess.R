source('../../lib/prepare.R')

require(testthat)

test_that("Preprocessing works", {
    data <-read.csv('preprocess-data.csv', na.strings = c(" ", "", "\t"))

    data <- RemoveHighMissing(data)
    data <- TreatNumeric(data)
    data <- TreatFactor(data)
    data <- ReduceLevels(data)
    data <- RemoveHighCorrelated(data)

    # Test that the numeric values were scaled
    expect_that(mean(data$Var1), equals(0, tolerance=0.01))

    # Test that the NA's were replaced
    expect_that(sum(is.na(data$Var1)), equals(0))
    expect_that(sum(is.na(data$Var6)), equals(0))

    # Test that the correlated columns were removed
    expect_that(sum(sapply(data, is.numeric)), equals(3))

    # Test that only the 10 most common levels were kept
    expect_that(length(levels(data$Var7)) < 11, is_true())
})
