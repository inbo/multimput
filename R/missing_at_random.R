#' Generate missing data at random
#'
#' The observed values will be either equal to the counts or missing.
#' The probability of missing is the inverse of the counts + 1.
#'
#' @param dataset A dataset to a the observation with missing data.
#' @param proportion The proportion of observations that will be missing.
#' @param count_variable The name of the variable holding the counts.
#' @param observed_variable The name of the variable holding the observed values
#' = either count or missing.
#' @export
missing_at_random <- function(
  dataset, proportion = 0.25, count_variable = "Count",
  observed_variable = "Observed"
) {
  n <- sample(
    nrow(dataset), size = ceiling((1 - proportion) * nrow(dataset)),
    replace = FALSE
  )
  dataset[-n, observed_variable] <- NA
  dataset[n, observed_variable] <- dataset[n, count_variable]
  return(dataset)
}
