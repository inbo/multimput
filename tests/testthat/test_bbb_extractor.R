context("extractor")
describe("extractor", {
  dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
  model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
  it("handles class which return a sensible summary(model)$coefficients", {
    expect_is(
      extractor(model),
      "matrix"
    )
    expect_identical(
      colnames(extractor(model)),
      c("Estimate", "Std. Error")
    )
    expect_identical(
      extractor(model),
      summary(model)$coefficients[, c("Estimate", "Std. Error")]
    )
  })

  it("returns an error when summary(model) doesn't contain coefficients", {
    model.aov <- aov(Count ~ factor(Period), data = dataset)
    expect_error(
      extractor(model.aov),
      "Currently, no default extractor\\(\\) for a aovlm model is available"
    )
  })

  it("returns an error when summary(model)$coefficients has wrong colnames", {
  })
})
