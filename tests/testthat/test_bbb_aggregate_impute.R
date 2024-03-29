test_that("aggregate_impute", {
  dataset <- generate_data(n_year = 10, n_site = 10, n_run = 1)
  dataset$Count[sample(nrow(dataset), 10)] <- NA
  dataset$Bottom <- 100000
  model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
  imputed <- impute(data = dataset, model = model)
  grouping <- c("Year", "Period")
  fun <- sum
  aggr <- aggregate_impute(imputed, grouping = grouping, fun = fun)

  # handles rawImputed
  expect_is(aggr, "aggregatedImputed")
  expect_identical(colnames(aggr@Covariate), grouping)
  expect_identical(nrow(aggr@Covariate), nrow(aggr@Imputation))
  expect_identical(ncol(imputed@Imputation), ncol(aggr@Imputation))

  # handles rawImputed with Minimum
  imputed2 <- impute(data = dataset, model = model, minimum = "Bottom")
  aggr2 <- aggregate_impute(imputed2, grouping = grouping, fun = fun)
  expect_is(aggr2, "aggregatedImputed")
  expect_identical(colnames(aggr2@Covariate), grouping)
  expect_identical(nrow(aggr2@Covariate), nrow(aggr2@Imputation))
  expect_identical(ncol(imputed2@Imputation), ncol(aggr2@Imputation))
  expect_true(all(aggr@Imputation <= aggr2@Imputation))

  # handles datasets without missing observations
  n_imp <- 19L
  dataset <- generate_data(n_year = 10, n_site = 50, n_run = 1)
  expect_identical(sum(is.na(dataset$Count)), 0L)
  model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
  imputed <- impute(model, dataset, n_imp = n_imp)
  grouping <- c("Year", "Period")
  fun <- sum
  aggr <- aggregate_impute(imputed, grouping = grouping, fun = fun)
  expect_is(aggr, "aggregatedImputed")
  apply(
    aggr@Imputation[, -1], 2,
    function(x) {
      expect_identical(
        x,
        aggr@Imputation[, 1]
      )
    }
  )

  # subsets the dataset
  aggr <- aggregate_impute(
    imputed, grouping = grouping, fun = fun, filter = list(~Year <= 5)
  )
  expect_lte(max(aggr@Covariate$Year), 5)
  aggr <- aggregate_impute(
    imputed, grouping = grouping, fun = fun, filter = list(~Year > 5)
  )
  expect_gt(min(aggr@Covariate$Year), 5)
  aggr <- aggregate_impute(
    imputed, grouping = grouping, fun = fun,
    join = data.frame(Year = seq(2L, 10L, by = 2L))
  )
  expect_identical(unique(aggr@Covariate$Year), seq(2L, 10L, by = 2L))

  # checks the sanity of the arguments
  expect_error(
    aggregate_impute(object = "junk"),
    "requires a 'rawImputed' or 'aggregatedImputed' object"
  )
  expect_error(
    aggregate_impute(imputed, grouping = "junk", fun = sum),
    "Column `junk` is not found"
  )
  expect_error(
    aggregate_impute(imputed, grouping = imputed),
    "grouping is not a character vector"
  )
  expect_error(
    aggregate_impute(imputed, grouping = NA),
    "grouping is not a character vector"
  )
  expect_error(
    aggregate_impute(imputed, grouping = "Year", fun = "junk"),
    "fun does not inherit from class function"
  )
  expect_error(
    aggregate_impute(imputed, grouping = "Year", fun = sum, filter = "junk"),
    "filter does not inherit from class list"
  )

  # aggregates an aggregatedImputed
  aggr <- aggregate_impute(imputed, grouping = grouping, fun = fun)
  expect_is(
    aggr2 <- aggregate_impute(aggr, grouping = "Year", fun = max),
    "aggregatedImputed"
  )
  expect_is(
    aggr2 <- aggregate_impute(
      aggr, grouping = "Year", fun = mean, filter = list("Period <= 3")
    ),
    "aggregatedImputed"
  )

  # handles empty datasets
  empty_imputed <- impute(data = dataset[integer(0), ], model = model)
  empty_aggr <- aggregate_impute(empty_imputed, grouping = grouping, fun = fun)
  expect_is(empty_aggr, "aggregatedImputed")
  expect_identical(colnames(empty_aggr@Covariate), grouping)
  expect_identical(nrow(empty_aggr@Covariate), nrow(empty_aggr@Imputation))
  expect_identical(ncol(empty_imputed@Imputation), ncol(empty_aggr@Imputation))
})

test_that("aggregate_impute() works on aggregatedImputed objects", {
  dataset <- generate_data(n_year = 10, n_site = 50, n_run = 1)
  dataset$Count[sample(nrow(dataset), 50)] <- NA
  dataset$Bottom <- 100000
  model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
  imputed <- impute(data = dataset, model = model)
  grouping <- c("Year", "Period")
  fun <- sum
  aggr <- aggregate_impute(imputed, grouping = grouping, fun = fun)
  grouping2 <- "Year"
  expect_is(
    aggr2 <- aggregate_impute(aggr, grouping = grouping2, fun = sum),
    "aggregatedImputed"
  )

  # handles empty datasets
  empty_imputed <- impute(data = dataset[integer(0), ], model = model)
  empty_aggr <- aggregate_impute(empty_imputed, grouping = grouping, fun = fun)
  empty_aggr2 <- aggregate_impute(empty_aggr, grouping = grouping2, fun = fun)
  expect_is(empty_aggr2, "aggregatedImputed")
  expect_identical(colnames(empty_aggr2@Covariate), grouping2)
  expect_identical(nrow(empty_aggr2@Covariate), nrow(empty_aggr2@Imputation))
  expect_identical(ncol(empty_aggr@Imputation), ncol(empty_aggr2@Imputation))
})
