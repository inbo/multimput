#' Generate missing data depending on the counts
#'
#' The observed values will be either equal to the counts or missing.
#' The probability of missing is the inverse of the counts + 1.
#'
#' @inheritParams missing_at_random
#' @export
missing_current_count <- function(
  dataset, proportion = 0.25, count_variable = "Count",
  observed_variable = "Observed"
) {
  dataset[, observed_variable] <- dataset[, count_variable]
  n <- sample(
    nrow(dataset),
    size = floor(proportion * nrow(dataset)),
    prob = 1 / (dataset[, count_variable] + 1)
  )
  dataset[n, observed_variable] <- NA
  return(dataset)
}
