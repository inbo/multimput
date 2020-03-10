#' Generate missing data mimicing choices made by volunteers.
#'
#' The observed values will be either equal to the counts or missing.
#' The probability of missing is the inverse of the counts + 1.
#'
#' @param dataset A dataset to a the observation with missing data.
#' @param proportion The proportion of observations that will be missing.
#' @param count.variable The name of the variable holding the counts.
#' @param observed.variable The name of the variable holding the observed values
#' = either count or missing.
#' @param year.variable The name of the variable holding the year.
#' @param site.variable The name of the variable holding the site.
#' @param max.count The maximum count.
#' @importFrom stats aggregate as.formula runif
#' @export
missingVolunteer <- function(
  dataset,
  proportion = 0.25,
  count.variable = "Count",
  observed.variable = "Observed",
  year.variable = "Year",
  site.variable = "Site",
  max.count = 100
) {
  sites <- factor(dataset[, site.variable])
  years <- sort(unique(dataset[, year.variable]))
  probability.start <- runif(length(levels(sites)))
  probability.continu <- sapply(seq(2, length(years), by = 1), function(i) {
    last.year <- which(dataset[, year.variable] == years[i - 1])
    observed.max <- aggregate(
      as.formula(paste(count.variable, site.variable, sep = "~")),
      data = dataset[last.year, c(site.variable, count.variable)],
      FUN = max
    )
    sqrt(pmin(observed.max[, count.variable], max.count) + 1) /
      sqrt(max.count + 1)
  })
  selected <- matrix(
    runif(
      length(probability.start) * length(years),
      max = probability.start
    ),
    ncol = length(years)
  )
  for (i in seq(2, length(years), by = 1)) {
    selected[, i] <- selected[, i] +
      selected[, i - 1] * probability.continu[, i - 1]
  }
  n <- sample(
    nrow(dataset),
    size = ceiling((1 - proportion) * nrow(dataset)),
    replace = FALSE,
    prob = selected[cbind(sites, years)]
  )
  dataset[-n, observed.variable] <- NA
  dataset[n, observed.variable] <- dataset[n, count.variable]
  return(dataset)
}
