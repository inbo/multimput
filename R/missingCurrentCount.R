#' Generate missing data depending on the counts
#' 
#' The observed values will be either equal to the counts or missing. The probability of missing is the inverse of the counts + 1.
#' 
#' @param dataset A dataset to a the observation with missing data
#' @param proportion The proportion of observations that will be missing
#' @param count.variable The name of the variable holding the counts
#' @param observed.variable The name of the variable holding the observed values = either count or missing
#' @export
missingCurrentCount <- function(dataset, proportion = 0.25, count.variable = "Count", observed.variable = "Observed"){
  dataset[, observed.variable] <- dataset[, count.variable]
  n <- sample(
    nrow(dataset), 
    size = floor(proportion * nrow(dataset)),
    prob = 1 / (dataset[, count.variable] + 1)
  )
  dataset[n, observed.variable] <- NA
  return(dataset)
}
