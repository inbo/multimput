#' Generate missing data at random
#'
#' The observed values will be either equal to the counts or missing.
#' The probability of missing is the inverse of the counts + 1.
#'
#' @param dataset A dataset to a the observation with missing data.
#' @param proportion The proportion of observations that will be missing.
#' @param count.variable The name of the variable holding the counts.
#' @param observed.variable The name of the variable holding the observed values
#' = either count or missing.
#' @export
missingAtRandom <- function(
  dataset,
  proportion = 0.25,
  count.variable = "Count",
  observed.variable = "Observed"
) {
  n <- sample(
    nrow(dataset),
    size = ceiling((1 - proportion) * nrow(dataset)),
    replace = FALSE
  )
  dataset[-n, observed.variable] <- NA
  dataset[n, observed.variable] <- dataset[n, count.variable]
  return(dataset)
}
