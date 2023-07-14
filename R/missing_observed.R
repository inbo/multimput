#' Generate missing data based on the observed patterns in the real dataset.
#'
#' The observed values will be either equal to the counts or missing.
#' The probability of missing is the inverse of the counts + 1.
#'
#' @inheritParams missing_at_random
#' @param site_variable The name of the variable holding the sites.
#' @param year_variable The name of the variable holding the years.
#' @param period_variable The name of the variable holding the period.
#' @importFrom utils data
#' @export
missing_observed <- function(
  dataset, count_variable = "Count", observed_variable = "Observed",
  site_variable = "Site", year_variable = "Year", period_variable = "Period"
) {

  sites <- dataset[, site_variable]
  if (!inherits(sites, "factor")) {
    sites <- factor(sites)
  }

  periods <- dataset[, period_variable]
  if (!inherits(periods, "factor")) {
    periods <- factor(periods)
  }

  data(waterfowl, envir = environment())
  waterfowl_site <- as.data.frame(table(Site = waterfowl$Site))
  selected_sites <- sample(
    as.integer(waterfowl_site$Site),
    length(levels(sites)),
    replace = TRUE
  )
  waterfowl <- waterfowl[waterfowl$Site %in% selected_sites, ]

  waterfowl_period <- as.data.frame(table(Period = waterfowl$Period))
  selected_periods <- sample(
    as.integer(waterfowl_period$Period),
    length(levels(periods)),
    replace = TRUE
  )
  waterfowl <- waterfowl[waterfowl$Period %in% selected_periods, ]

  combination <- unique(cbind(sites, periods))
  pattern <- apply(combination, 1, function(x) {
    selection <- waterfowl$Site == selected_sites[x["sites"]] &
      waterfowl$Period == selected_periods[x["periods"]]
    if (sum(selection) == 0) {
      return(NULL)
    }
    data.frame(
      waterfowl[selection, "Winter"],
      as.vector(x["sites"]),
      as.vector(x["periods"])
    )
  })
  pattern <- do.call(rbind, pattern)
  colnames(pattern) <- c(year_variable, site_variable, period_variable)
  pattern[, site_variable] <- factor(
    pattern[, site_variable],
    levels = sort(unique(sites)),
    labels = levels(sites)
  )
  pattern[, period_variable] <- factor(
    pattern[, period_variable],
    levels = sort(unique(periods)),
    labels = levels(periods)
  )
  if (!inherits(dataset[, year_variable], "factor")) {
    pattern[, year_variable] <- factor(
      pattern[, year_variable],
      levels = sort(unique(dataset[, year_variable])),
      labels = levels(dataset[, year_variable])
    )
  }
  pattern[, observed_variable] <- 1

  dataset <- merge(dataset, pattern, all.x = TRUE)

  observed <- !is.na(dataset[, observed_variable])

  dataset[observed, observed_variable] <- dataset[observed, count_variable]

  return(dataset)
}
