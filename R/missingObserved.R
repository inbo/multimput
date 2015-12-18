#' Generate missing data based on the observed patterns in the real dataset.
#'
#' The observed values will be either equal to the counts or missing. The probability of missing is the inverse of the counts + 1.
#'
#' @param dataset A dataset to a the observation with missing data
#' @param count.variable The name of the variable holding the counts
#' @param observed.variable The name of the variable holding the observed values = either count or missing
#' @param site.variable The name of the variable holding the sites
#' @param year.variable The name of the variable holding the years
#' @param period.variable The name of the variable holding the period
#' @export
missingObserved <- function(
  dataset,
  count.variable = "Count",
  observed.variable = "Observed",
  site.variable = "Site",
  year.variable = "Year",
  period.variable = "Period"
){

  sites <- dataset[, site.variable]
  if (class(sites) != "factor") {
    sites <- factor(sites)
  }

  periods <- dataset[, period.variable]
  if (class(periods) != "factor") {
    periods <- factor(periods)
  }

  data(waterfowl, envir = environment())
  waterfowl.site <- as.data.frame(table(Site = waterfowl$Site))
  selected.sites <- sample(
    as.integer(waterfowl.site$Site),
    length(levels(sites)),
    replace = TRUE
  )
  waterfowl <- waterfowl[waterfowl$Site %in% selected.sites, ]

  waterfowl.period <- as.data.frame(table(Period = waterfowl$Period))
  selected.periods <- sample(
    as.integer(waterfowl.period$Period),
    length(levels(periods)),
    replace = TRUE
  )
  waterfowl <- waterfowl[waterfowl$Period %in% selected.periods, ]

  combination <- unique(cbind(sites, periods))
  pattern <- apply(combination, 1, function(x){
    selection <- waterfowl$Site == selected.sites[x["sites"]] &
      waterfowl$Period == selected.periods[x["periods"]]
    if (sum(selection) == 0) {
      return(NULL)
    } else {
      data.frame(
        waterfowl[selection, "Winter"],
        as.vector(x["sites"]),
        as.vector(x["periods"])
      )
    }
  })
  pattern <- do.call(rbind, pattern)
  colnames(pattern) <- c(year.variable, site.variable, period.variable)
  pattern[, site.variable] <- factor(
    pattern[, site.variable],
    levels = sort(unique(sites)),
    labels = levels(sites)
  )
  pattern[, period.variable] <- factor(
    pattern[, period.variable],
    levels = sort(unique(periods)),
    labels = levels(periods)
  )
  if (class(dataset[, year.variable]) == "factor") {
    pattern[, year.variable] <- factor(
      pattern[, year.variable],
      levels = sort(unique(dataset[, year.variable])),
      labels = levels(dataset[, year.variable])
    )
  }
  pattern[, observed.variable] <- 1

  dataset <- merge(dataset, pattern, all.x = TRUE)

  observed <- !is.na(dataset[, observed.variable])

  dataset[observed, observed.variable] <- dataset[observed, count.variable]

  return(dataset)
}
