context("aggregate_impute")
describe("aggregate_impute", {
  dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
  dataset$Count[sample(nrow(dataset), 50)] <- NA
  model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
  imputed <- impute(data = dataset, model = model)
  grouping <- c("Year", "Period")
  fun <- sum
  it("handles rawImputed", {
    expect_is(
      aggr <- aggregate_impute(imputed, grouping = grouping, fun = fun),
      "aggregatedImputed"
    )
    expect_identical(
      colnames(aggr@Covariate),
      grouping
    )
    expect_identical(
      nrow(aggr@Covariate),
      nrow(aggr@Imputation)
    )
    expect_identical(
      ncol(imputed@Imputation),
      ncol(aggr@Imputation)
    )
  })
})
