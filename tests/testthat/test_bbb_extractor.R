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
    # see R/zzz_test_extractor.R for the required auxilary functions
    # this hack required to make the S3 methods available for the unit tests
    junk <- "junk"
    class(junk) <- "N"
    expect_error(
      extractor(junk),
      "Currently, no default extractor\\(\\) for a N model is available"
    )
    class(junk) <- "E"
    expect_error(
      extractor(junk),
      "Currently, no default extractor\\(\\) for a E model is available"
    )
    class(junk) <- "S"
    expect_error(
      extractor(junk),
      "Currently, no default extractor\\(\\) for a S model is available"
    )
    class(junk) <- "C"
    expect_equal(
      extractor(junk),
      matrix(
        0,
        ncol = 2,
        dimnames = list(NULL, c("Estimate", "Std. Error"))
      )
    )
  })
})
