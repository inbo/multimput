#' Generate missing data mimicing choices made by volunteers.
#'
#' The observed values will be either equal to the counts or missing.
#' The probability of missing is the inverse of the counts + 1.
#'
#' @inheritParams missing_at_random
#' @inheritParams missing_observed
#' @param max_count The maximum count.
#' @importFrom stats aggregate as.formula runif
#' @export
missing_volunteer <- function(
  dataset, proportion = 0.25, count_variable = "Count",
  observed_variable = "Observed", year_variable = "Year",
  site_variable = "Site", max_count = 100
) {
  sites <- factor(dataset[, site_variable])
  years <- sort(unique(dataset[, year_variable]))
  probability_start <- runif(length(levels(sites)))
  probability_continu <- sapply(seq(2, length(years), by = 1), function(i) {
    last.year <- which(dataset[, year_variable] == years[i - 1])
    observed.max <- aggregate(
      as.formula(paste(count_variable, site_variable, sep = "~")),
      data = dataset[last.year, c(site_variable, count_variable)],
      FUN = max
    )
    sqrt(pmin(observed.max[, count_variable], max_count) + 1) /
      sqrt(max_count + 1)
  })
  selected <- matrix(
    runif(
      length(probability_start) * length(years),
      max = probability_start
    ),
    ncol = length(years)
  )
  for (i in seq(2, length(years), by = 1)) {
    selected[, i] <- selected[, i] +
      selected[, i - 1] * probability_continu[, i - 1]
  }
  n <- sample(
    nrow(dataset),
    size = ceiling((1 - proportion) * nrow(dataset)),
    replace = FALSE,
    prob = selected[cbind(sites, years)]
  )
  dataset[-n, observed_variable] <- NA
  dataset[n, observed_variable] <- dataset[n, count_variable]
  return(dataset)
}
