context("model_impute")
describe("model_impute", {
  dataset <- generateData(n.year = 10, n.site = 10, n.run = 1)
  it("has no effect when there are no missing values", {
    model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
    imputed <- impute(data = dataset, model = model)
    aggr <- aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
    extractor <- function(model){
      summary(model)$coefficients[, c("Estimate", "Std. Error")]
    }
    model.aggr <- model_impute(
      aggr,
      model.fun = lm,
      rhs = "0 + factor(Year)",
      extractor = extractor
    )
    aggr.base <- aggregate(Count ~ Year + Period, data = dataset, FUN = sum)
    model.base <- lm(Count ~ 0 + factor(Year), data = aggr.base)
    expect_equal(
      unname(as.matrix(model.aggr[, 2:3])),
      unname(extractor(model.base))
    )
  })

  dataset$Count[sample(nrow(dataset), 10)] <- NA
  model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
  imputed <- impute(data = dataset, model = model)
  aggr <- aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
  extractor <- function(model){
    summary(model)$coefficients[, c("Estimate", "Std. Error")]
  }
  it("handles rawImputed", {
    expect_is(
      model.imp <- model_impute(
        aggr,
        model.fun = lm,
        rhs = "0 + factor(Year)",
        extractor = extractor
      ),
      "data.frame"
    )
    expect_identical(
      colnames(model.imp),
      c("Parameter", "Estimate", "SE", "LCL", "UCL")
    )
  })
  it("checks the sanity of the arguments", {
    expect_error(
      model_impute(object = "junk"),
      "model_impute\\(\\) doesn't handle a 'character' object"
    )
    dataset <- generateData(n.year = 10, n.site = 10, n.run = 1)
    dataset$Count[sample(nrow(dataset), 10)] <- NA
    model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
    imputed <- impute(data = dataset, model = model)
    aggr <- aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
    expect_error(
      model_impute(
        aggr,
        model.fun = "junk",
        rhs = "0 + factor(Year)"
      ),
      "model.fun does not inherit from class function"
    )
    expect_error(
      model_impute(
        aggr,
        model.fun = lm,
        rhs = "0 + factor(Year)",
        extractor = "junk"
      ),
      "extractor does not inherit from class function"
    )
    extractor <- function(model){
      summary(model)$coefficients[, c("Estimate", "Std. Error")]
    }
    expect_error(
      model_impute(
        aggr,
        model.fun = lm,
        rhs = "0 + factor(Year)",
        model.args = "junk",
        extractor = extractor
      ),
      "model.args does not inherit from class list"
    )
    expect_error(
      model_impute(
        aggr,
        model.fun = lm,
        rhs = "0 + factor(Year)",
        extractor.args = "junk",
        extractor = extractor
      ),
      "extractor.args does not inherit from class list"
    )
    expect_error(
      model_impute(
        aggr,
        model.fun = lm,
        rhs = NA,
        extractor = extractor
      ),
      "rhs is not a character vector"
    )
    expect_error(
      model_impute(
        aggr,
        model.fun = lm,
        rhs = ~factor(Year),
        extractor = extractor
      ),
      "rhs is not a character vector"
    )
    expect_error(
      model_impute(
        aggr,
        model.fun = lm,
        rhs = "junk",
        extractor = extractor
      ),
      "model failed on all imputations"
    )
  })
})
