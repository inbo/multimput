context("model_impute")
describe("model_impute", {
  dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
  dataset$Count[sample(nrow(dataset), 50)] <- NA
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
      "matrix"
    )
    expect_identical(
      colnames(model.imp),
      c("Estimate", "SE")
    )
  })
})
