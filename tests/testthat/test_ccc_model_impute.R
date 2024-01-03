dataset <- generate_data(n_year = 10, n_site = 10, n_run = 1)
test_that("model_impute has no effect when there are no missing values", {
  model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
  imputed <- impute(data = dataset, model = model)
  aggr <- aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
  extractor <- function(model) {
    summary(model)$coefficients[, c("Estimate", "Std. Error")]
  }
  model_aggr <- model_impute(
    aggr, model_fun = lm, rhs = "0 + factor(Year)", extractor = extractor
  )
  aggr_base <- aggregate(Count ~ Year + Period, data = dataset, FUN = sum)
  model_base <- lm(Count ~ 0 + factor(Year), data = aggr_base)
  expect_equal(
    unname(as.matrix(model_aggr[, 2:3])), unname(extractor(model_base))
  )
})

test_that("model_impute, handles rawImputed", {
  dataset$Count[sample(nrow(dataset), 10)] <- NA
  model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
  imputed <- impute(data = dataset, model = model)
  aggr <- aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
  extractor <- function(model) {
    summary(model)$coefficients[, c("Estimate", "Std. Error")]
  }
  expect_is(
    model_imp <- model_impute(
      aggr, model_fun = lm, rhs = "0 + factor(Year)", extractor = extractor
    ),
    "data.frame"
  )
  expect_identical(
    colnames(model_imp), c("Parameter", "Estimate", "SE", "LCL", "UCL")
  )
})
test_that("model_impute checks the sanity of the arguments", {
  expect_error(
    model_impute(object = "junk"), "doesn't handle a 'character' object"
  )
  dataset <- generate_data(n_year = 10, n_site = 10, n_run = 1)
  dataset$Count[sample(nrow(dataset), 10)] <- NA
  model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
  imputed <- impute(data = dataset, model = model)
  aggr <- aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
  expect_error(
    model_impute(aggr, model_fun = list("junk"), rhs = "0 + factor(Year)"),
    "model_fun does not inherit from class function"
  )
  expect_error(
    model_impute(
      aggr, model_fun = lm, rhs = "0 + factor(Year)", extractor = "junk"
    ),
    "extractor does not inherit from class function"
  )
  extractor <- function(model) {
    summary(model)$coefficients[, c("Estimate", "Std. Error")]
  }
  expect_error(
    model_impute(
      aggr, model_fun = lm, rhs = "0 + factor(Year)", model_args = "junk",
      extractor = extractor
    ),
    "model_args does not inherit from class list"
  )
  expect_error(
    model_impute(
      aggr, model_fun = lm, rhs = "0 + factor(Year)", extractor_args = "junk",
      extractor = extractor
    ),
    "extractor_args does not inherit from class list"
  )
  expect_error(
    model_impute(aggr, model_fun = lm, rhs = NA, extractor = extractor),
    "rhs is not a character vector"
  )
  expect_error(
    model_impute(
      aggr, model_fun = lm, rhs = ~factor(Year), extractor = extractor
    ),
    "rhs is not a character vector"
  )
  expect_error(
    model_impute(aggr, model_fun = lm, rhs = "junk", extractor = extractor),
    "model failed on all imputations"
  )
})

test_that("model_impute handles empty datasets", {
  model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
  imputed <- impute(data = dataset, model = model)
  aggr <- aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
  extractor <- function(model) {
    summary(model)$coefficients[, c("Estimate", "Std. Error")]
  }

  empty <- aggr
  empty@Covariate <- empty@Covariate[0, ]
  model_aggr <- model_impute(
    empty, model_fun = lm, rhs = "0 + factor(Year)", extractor = extractor
  )
  expect_s3_class(model_aggr, "data.frame")
  expect_equal(nrow(model_aggr), 0)
  expect_identical(
    colnames(model_aggr), c("Parameter", "Estimate", "SE", "LCL", "UCL")
  )

  model_aggr <- model_impute(
    aggr, model_fun = "stats::lm", rhs = "0 + factor(Year)", extractor = extractor,
    filter = function(x) {
      return(x[0, ])
    }
  )
  expect_s3_class(model_aggr, "data.frame")
  expect_equal(nrow(model_aggr), 0)
  expect_identical(
    colnames(model_aggr), c("Parameter", "Estimate", "SE", "LCL", "UCL")
  )
})
