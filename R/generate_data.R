#' Generate simulated data
#'
#' Generate data for a regural monitoring design.
#' The counts follow a negative binomial distribution with given size parameters
#' and the true mean mu depending on a year, period and site effect.
#' All effects are independent from each other and have, on the log-scale, a
#' normal distribution with zero mean and given standard deviation.
#' @param intercept The global mean on the log-scale.
#' @param n_year The number of years.
#' @param n_period The number of periods.
#' @param n_site The number of sites.
#' @param year_factor Convert year to a factor.
#' Defaults to `FALSE`.
#' @param period_factor Convert period to a factor.
#' Defaults to `FALSE`.
#' @param site_factor Convert site to a factor.
#' Defaults to `FALSE`.
#' @param trend The longterm linear trend on the log-scale.
#' @param sd_rw_year The standard deviation of the year effects on the
#' log-scale.
#' @param amplitude_period The amplitude of the periodic effect on the
#' log-scale.
#' @param mean_phase_period The mean of the phase of the periodic effect among
#' years.
#' Defaults to `0`.
#' @param sd_phase_period The standard deviation of the phase of the periodic
#' effect among years.
#' @param sd_site The standard deviation of the site effects on the log-scale.
#' @param sd_rw_site The standard deviation of the random walk along year per
#' site on the log-scale.
#' @param sd_noise The standard deviation of the noise effects on the log-scale.
#' @param size The size parameter of the negative binomial distribution.
#' @param n_run The number of runs with the same mu.
#' @param as.list Return the dataset as a list rather than a data.frame.
#' Defaults to `FALSE`.
#' @param details Add variables containing the year, period and site effects.
#' Defaults tot `FALSE`.
#' @export
#' @return A `data.frame` with five variables.
#' `Year`, `Month` and `Site` are factors identifying the location and time of
#' monitoring.
#' `Mu` is the true mean of the negative binomial distribution in the original
#' scale.
#' `Count` are the simulated counts.
#' @importFrom stats rnorm rnbinom
#' @importFrom dplyr %>% group_by group_map mutate select
#' @importFrom rlang .data !!!
generate_data <- function(
  intercept = 2, n_year = 24, n_period = 6, n_site = 20, year_factor = FALSE,
  period_factor = FALSE, site_factor = FALSE, trend = 0.01, sd_rw_year = 0.1,
  amplitude_period = 1, mean_phase_period = 0, sd_phase_period = 0.2,
  sd_site = 1, sd_rw_site = 0.02, sd_noise = 0.01, size = 2, n_run = 10,
  as_list = FALSE, details = FALSE
) {
  #generate the design
  dataset <- expand.grid(
    Year = seq_len(n_year),
    Period = seq_len(n_period),
    Site = seq_len(n_site)
  )
  year_rw_effect <- cumsum(rnorm(n_year, mean = 0, sd = sd_rw_year))
  phase <- rnorm(n_year + 1, mean = mean_phase_period, sd = sd_phase_period)
  site_rw_effect <- rbind(
    rnorm(
      n_site,
      mean = 0,
      sd = sd_site
    ),
    matrix(
      rnorm(
        (n_year - 1) * n_site,
        mean = 0,
        sd = sd_rw_site
      ),
      ncol = n_site
    )
  )
  site_rw_effect <- as.vector(apply(site_rw_effect, 2, cumsum))
  observation_effect <- rnorm(nrow(dataset), mean = 0, sd = sd_noise)

  #generate the true mean
  dataset$Mu <- exp(
    intercept +
      year_rw_effect[dataset$Year] + trend * dataset$Year +
      amplitude_period *
        sin(
          dataset$Period * pi / n_period + phase[dataset$Year]
        ) +
      site_rw_effect[dataset$Year + (dataset$Site - 1) * n_year] +
      observation_effect
  )

  if (details) {
    dataset$YearEffect <- exp(
      year_rw_effect[dataset$Year] + trend * dataset$Year
    )
    dataset$PeriodEffect <- exp(
      amplitude_period *
        sin(
          dataset$Period * pi / n_period + phase[dataset$Year]
        )
    )
    dataset$SiteEffect <- exp(
      site_rw_effect[dataset$Year + (dataset$Site - 1) * n_year]
    )
  }
  if (year_factor) {
    dataset$Year <- factor(dataset$Year)
  }
  if (period_factor) {
    dataset$Period <- factor(dataset$Period)
  }
  if (site_factor) {
    dataset$Site <- factor(dataset$Site)
  }
  # add the runs
  dataset <- merge(
    dataset,
    data.frame(
      Run = seq_len(n_run)
    )
  )

  #generate the counts
  dataset$Count <- rnbinom(nrow(dataset), size = size, mu = dataset$Mu)

  if (!as.list) {
    return(dataset)
  }

  dataset %>%
    group_by(.data$Run) %>%
    group_map(~relevant(.x, details = details, run = .y))
}

# internal function for generate_data()
relevant <- function(x, details, run) {
  if (details) {
    dots <- c("Year", "Period", "Site", "Mu", "YearEffect", "PeriodEffect",
              "SiteEffect", "Count")
  } else {
    dots <- c("Year", "Period", "Site", "Mu", "Count")
  }
  x %>%
    select(!!!dots) %>%
    mutate(Run = run$Run) %>%
    as.data.frame()
}
