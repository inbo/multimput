describe("impute", {
  context("impute")
  dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
  dataset$Count[sample(nrow(dataset), 50)] <- NA
  n.imp <- 50L
  it("handles lm", {
    model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
    expect_is(
      imputed <- impute(model, dataset),
      "rawImputed"
    )
    expect_identical(
      ncol(imputed@Imputation),
      19L
    )
    expect_identical(
      nrow(imputed@Imputation),
      sum(is.na(dataset$Count))
    )

    expect_is(
      imputed <- impute(model, dataset, n.imp = n.imp),
      "rawImputed"
    )
    expect_identical(
      ncol(imputed@Imputation),
      n.imp
    )
    expect_identical(
      nrow(imputed@Imputation),
      sum(is.na(dataset$Count))
    )
  })

  it("handles datasets without missing observations", {
    n.imp <- 19L
    dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
    expect_identical(
      sum(is.na(dataset$Count)),
      0L
    )
    model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
    expect_is(
      imputed <- impute(model, dataset, n.imp = n.imp),
      "rawImputed"
    )
    expect_identical(
      ncol(imputed@Imputation),
      n.imp
    )
    expect_identical(
      nrow(imputed@Imputation),
      sum(is.na(dataset$Count))
    )
  })
})
