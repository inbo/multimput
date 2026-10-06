#' Create a small aggregated dataset with grouping columns and imputations.
#' The helper keeps the tests short and makes the expected values explicit.
aggregate_clamp_data <- function() {
  data.frame(
    Year = c(1L, 1L, 2L, 2L),
    Period = c(1L, 2L, 1L, 2L),
    Imputation0001 = c(10, 20, 30, 40),
    Imputation0002 = c(15, 25, 35, 45)
  )
}

test_that("aggregate_clamp() returns the totals unchanged without `minimum`", {
  total <- aggregate_clamp_data()
  expect_identical(aggregate_clamp(total), total)

  # other arguments passed through `...` must be ignored
  expect_identical(aggregate_clamp(total, junk = 1), total)
})

test_that("aggregate_clamp() clamps the totals to the minimum", {
  total <- aggregate_clamp_data()
  minimum <- data.frame(
    Year = c(1L, 1L, 2L, 2L),
    Period = c(1L, 2L, 1L, 2L),
    Floor = c(100, 0, 100, 0)
  )
  clamped <- aggregate_clamp(total, minimum = minimum)

  expect_identical(
    colnames(clamped),
    c("Year", "Period", "Imputation0001", "Imputation0002")
  )
  expect_identical(nrow(clamped), nrow(total))
  # groups with a floor of 100 are raised, groups with a floor of 0 are kept
  expect_identical(clamped$Imputation0001, c(100, 20, 100, 40))
  expect_identical(clamped$Imputation0002, c(100, 25, 100, 45))

  # `NA` in the minimum must not propagate into the imputations
  minimum$Floor[1] <- NA
  expect_identical(
    aggregate_clamp(total, minimum = minimum)$Imputation0001,
    c(10, 20, 100, 40)
  )
})

test_that("aggregate_clamp() checks the sanity of `minimum`", {
  total <- aggregate_clamp_data()
  minimum <- data.frame(
    Year = c(1L, 1L, 2L, 2L),
    Period = c(1L, 2L, 1L, 2L),
    Floor = 1
  )

  expect_error(
    aggregate_clamp(total, minimum = "junk"),
    "`minimum` is not a data.frame"
  )
  expect_error(
    aggregate_clamp(total, minimum = minimum[, c("Year", "Floor")]),
    "`minimum` does not contain all grouping columns"
  )
  minimum$Extra <- 1
  expect_error(
    aggregate_clamp(total, minimum = minimum),
    "`minimum` should have exactly one more column than the grouping columns"
  )
  minimum$Extra <- NULL
  expect_error(
    aggregate_clamp(total, minimum = rbind(minimum, minimum)),
    "`minimum` contains duplicate rows for the grouping columns"
  )
})
