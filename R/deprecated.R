#' Deprecated functions
#'
#' These functions will be removed from `multimput` in the future.
#' @inheritParams generate_data
#' @param n.year The number of years.
#' @param n.period The number of periods.
#' @param n.site The number of sites.
#' @param year.factor Convert year to a factor.
#' Defaults to `FALSE`.
#' @param period.factor Convert period to a factor.
#' Defaults to `FALSE`.
#' @param site.factor Convert site to a factor.
#' Defaults to `FALSE`.
#' @param sd.rw.year The standard deviation of the year effects on the
#' log-scale.
#' @param amplitude.period The amplitude of the periodic effect on the
#' log-scale.
#' @param mean.phase.period The mean of the phase of the periodic effect among
#' years.
#' Defaults to `0`.
#' @param sd.phase.period The standard deviation of the phase of the periodic
#' effect among years.
#' @param sd.site The standard deviation of the site effects on the log-scale.
#' @param sd.rw.site The standard deviation of the random walk along year per
#' site on the log-scale.
#' @param sd.noise The standard deviation of the noise effects on the log-scale.
#' @param n.run The number of runs with the same mu.
#' @param as.list Return the dataset as a list rather than a data.frame.
#' Defaults to `FALSE`.
#' @export
#' @rdname deprecated
generateData <- function( # nolint: object_name_linter.
    intercept = 2, n.year = 24, n.period = 6, n.site = 20, year.factor = FALSE, # nolint: object_name_linter, line_length_linter.
    period.factor = FALSE, site.factor = FALSE, trend = 0.01, sd.rw.year = 0.1, # nolint: object_name_linter, line_length_linter.
    amplitude.period = 1, mean.phase.period = 0, sd.phase.period = 0.2, # nolint: object_name_linter, line_length_linter.
    sd.site = 1, sd.rw.site = 0.02, sd.noise = 0.01, size = 2, n.run = 10, # nolint: object_name_linter, line_length_linter.
    as.list = FALSE, details = FALSE # nolint: object_name_linter.
) {
  .Deprecated("generate_data", package = "multimput")
  generate_data(
    intercept = intercept, n_year = n.year, n_period = n.period, # nolint: object_name_linter, line_length_linter.
    n_site = n.site, year_factor = year.factor, period_factor = period.factor, # nolint: object_name_linter, line_length_linter.
    site_factor = site.factor, trend = trend, sd_rw_year = sd.rw.year, # nolint: object_name_linter, line_length_linter.
    amplitude_period = amplitude.period, mean_phase_period = mean.phase.period, # nolint: object_name_linter, line_length_linter.
    sd_phase_period = sd.phase.period, sd_site = sd.site, # nolint: object_name_linter, line_length_linter.
    sd_rw_site = sd.rw.site, sd_noise = sd.noise, size = size, n_run = n.run, # nolint: object_name_linter, line_length_linter.
    as_list = as.list, details = details # nolint: object_name_linter.
  )
}

#' @inheritParams missing_at_random
#' @param count.variable The name of the variable holding the counts.
#' @param observed.variable The name of the variable holding the observed values
#' = either count or missing.
#' @export
#' @rdname deprecated
missingAtRandom <- function( # nolint: object_name_linter.
    dataset, proportion = 0.25, count.variable = "Count", # nolint: object_name_linter, line_length_linter.
    observed.variable = "Observed" # nolint: object_name_linter.
) {
  .Deprecated("missing_at_random", package = "multimput")
  missing_at_random(
    dataset = dataset, proportion = proportion, count_variable = count.variable, # nolint: object_name_linter, line_length_linter.
    observed_variable = observed.variable # nolint: object_name_linter.
  )
}
