describe("impute", {
  context("impute")
  dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
  dataset$Count[sample(nrow(dataset), 50)] <- NA
  dataset$fYear <- factor(dataset$Year)
  dataset$fPeriod <- factor(dataset$Period)
  dataset$fSite <- factor(dataset$Site)
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







  it("handles inla with poisson distribution", {
    if (!require(INLA)) {
      skip("INLA package not available")
    }
    model <- INLA::inla(
      Count ~ factor(Year) + factor(Period) + f(Site, model = "iid"),
      data = dataset,
      family = "poisson",
      control.predictor = list(compute = TRUE)
    )
    expect_is(
      imputed <- impute(model),
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





  it("handles inla with negative binomial distribution", {
    if (!require(INLA)) {
      skip("INLA package not available")
    }
    model <- INLA::inla(
      Count ~ factor(Year) + factor(Period) + f(Site, model = "iid"),
      data = dataset,
      family = "nbinomial",
      control.predictor = list(compute = TRUE)
    )
    expect_is(
      imputed <- impute(model),
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





  it("is robust for wrong imput", {
    expect_error(
      impute(model = "junk"),
      "impute\\(\\) can't handle a model of class character at this moment."
    )
    model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
    expect_error(
      impute(model = model, data = "junk"),
      "data does not inherit from class data.frame"
    )
    expect_error(
      impute(model = model, data = dataset, n.imp = -1),
      "n.imp is not a count \\(a single positive integer\\)"
    )
    expect_error(
      impute(model = model, data = dataset, n.imp = 0),
      "n.imp is not a count \\(a single positive integer\\)"
    )
    expect_error(
      impute(model = model, data = dataset, n.imp = "junk"),
      "n.imp is not a count \\(a single positive integer\\)"
    )
    wrong.dataset <- dataset
    wrong.dataset$Year <- NULL
    expect_error(
      impute(model = model, data = wrong.dataset),
      "object 'Year' not found"
    )
    wrong.dataset <- dataset
    wrong.dataset$Count <- NULL
    expect_error(
      impute(model = model, data = wrong.dataset),
      "data does not have name Count"
    )

    if (!require(INLA)) {
      skip("INLA package not available")
    }
    model <- INLA::inla(
      Mu ~ factor(Year) + factor(Period) + f(Site, model = "iid"),
      data = dataset,
      family = "gamma"
    )
    expect_error(
      impute(model),
"model must be fit with the 'compute = TRUE' argument of control.predictor"
    )
    model <- INLA::inla(
      Mu ~ factor(Year) + factor(Period) + f(Site, model = "iid"),
      data = dataset,
      family = "gamma",
      control.predictor = list(compute = TRUE)
    )
    expect_error(
      impute(model),
      "Imputations from the 'gamma' family not yet defined"
    )

    model <- lme4::glmer(
      Count ~ factor(Year) + fPeriod + (1 | fSite),
      data = dataset,
      family = poisson
    )
    expect_error(
      impute(model, dataset),
      "impute can't handle factor\\(\\) in the model"
    )
    expect_error(
      impute(model = model, data = "junk"),
      "data does not inherit from class data.frame"
    )
    expect_error(
      impute(model = model, data = dataset, n.imp = -1),
      "n.imp is not a count \\(a single positive integer\\)"
    )
    expect_error(
      impute(model = model, data = dataset, n.imp = 0),
      "n.imp is not a count \\(a single positive integer\\)"
    )
    expect_error(
      impute(model = model, data = dataset, n.imp = "junk"),
      "n.imp is not a count \\(a single positive integer\\)"
    )

    model <- lme4::glmer(
      Count ~ fYear + fPeriod + (1 | fSite),
      data = dataset,
      family = poisson
    )
    wrong.dataset <- dataset
    wrong.dataset$fPeriod <- NULL
    expect_error(
      impute(model = model, data = wrong.dataset),
      "object 'fPeriod' not found"
    )
    wrong.dataset <- dataset
    wrong.dataset$Count <- NULL
    expect_error(
      impute(model = model, data = wrong.dataset),
      "data does not have name Count"
    )
  })


  dataset <- generateData(
    n.year = 10,
    n.site = 50,
    n.run = 1,
    year.factor = TRUE,
    period.factor = TRUE,
    site.factor = TRUE
  )
  dataset$Count[sample(nrow(dataset), 50)] <- NA
  it("handles glmerMod objects", {
    model <- lme4::glmer(
      Count ~ Year + Period + (1 | Site),
      data = dataset,
      family = poisson
    )
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
})
