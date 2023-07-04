test_that("hurdle_impute", {
  set.seed(20230704)
  dataset <- generate_data(n_year = 10, n_site = 10, n_run = 1, intercept = 1)
  dataset |>
    mutate(
      Minimum = .data$Count,
      Count = ifelse(.data$Count > 0, .data$Count, NA)
    ) -> count
  model <- lme4::glmer(Count ~ 1 + (1 | Year), data = count, family = poisson)
  impute_count <- impute(model, data = count, minimum = "Minimum")
  dataset |>
    mutate(
      Minimum = as.integer(.data$Count > 0),
      Present = map_int(
        .data$Count, ~ifelse(.x > 0, 1, sample(c(0, NA), size = 1))
      )
    ) -> present
  model <- lme4::glmer(
    Present ~ 1 + (1 | Year), data = present, family = binomial
  )
  impute_present <- impute(model, data = present, minimum = "Minimum")
  expect_is(
    impute_hurdle <- hurdle_impute(impute_present, impute_count),
    "aggregatedImputed"
  )
})
