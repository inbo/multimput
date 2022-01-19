context("generate imputed values")
describe("impute", {
  context("impute")
  dataset <- generate_data(n_year = 10, n_site = 10, n_run = 1)
  dataset$Count[sample(nrow(dataset), 10)] <- NA
  dataset$fYear <- factor(dataset$Year)
  dataset$fPeriod <- factor(dataset$Period)
  dataset$fSite <- factor(dataset$Site)
  dataset$Bottom <- 10000
  n_imp <- 10L
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
    expect_identical(
      imputed@Minimum,
      ""
    )

    expect_is(
      imputed <- impute(model, dataset, n_imp = n_imp),
      "rawImputed"
    )
    expect_identical(
      ncol(imputed@Imputation),
      n_imp
    )
    expect_identical(
      nrow(imputed@Imputation),
      sum(is.na(dataset$Count))
    )

    expect_is(
      imputed <- impute(model, dataset, minimum = "Bottom"),
      "rawImputed"
    )
    expect_identical(
      imputed@Minimum,
      "Bottom"
    )

    expect_error(
      impute(model, dataset, minimum = "Junk"),
      "object@Data does not have .*name.*Junk"
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
      control.compute = list(config = TRUE),
      control.predictor = list(compute = TRUE, link = 1)
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
    expect_identical(
      imputed@Minimum,
      ""
    )

    expect_is(
      imputed <- impute(model, dataset, n_imp = n_imp),
      "rawImputed"
    )
    expect_identical(
      ncol(imputed@Imputation),
      n_imp
    )
    expect_identical(
      nrow(imputed@Imputation),
      sum(is.na(dataset$Count))
    )

    expect_is(
      imputed <- impute(model, dataset, minimum = "Bottom"),
      "rawImputed"
    )
    expect_identical(
      imputed@Minimum,
      "Bottom"
    )

    expect_error(
      impute(model, dataset, minimum = "Junk"),
      "object@Data does not have .*name.*Junk"
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
      control.compute = list(config = TRUE),
      control.predictor = list(compute = TRUE, link = 1)
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
    expect_identical(
      imputed@Minimum,
      ""
    )

    expect_is(
      imputed <- impute(model, dataset, n_imp = n_imp),
      "rawImputed"
    )
    expect_identical(
      ncol(imputed@Imputation),
      n_imp
    )
    expect_identical(
      nrow(imputed@Imputation),
      sum(is.na(dataset$Count))
    )

    expect_is(
      imputed <- impute(model, dataset, minimum = "Bottom"),
      "rawImputed"
    )
    expect_identical(
      imputed@Minimum,
      "Bottom"
    )

    expect_error(
      impute(model, dataset, minimum = "Junk"),
      "object@Data does not have.*name.*Junk"
    )
  })





  it("handles datasets without missing observations", {
    n_imp <- 19L
    dataset <- generateData(n_year = 10, n_site = 50, n_run = 1)
    dataset$Bottom <- 10000
    expect_identical(
      sum(is.na(dataset$Count)),
      0L
    )
    model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
    expect_is(
      imputed <- impute(model, dataset, n_imp = n_imp),
      "rawImputed"
    )
    expect_identical(
      ncol(imputed@Imputation),
      n_imp
    )
    expect_identical(
      nrow(imputed@Imputation),
      sum(is.na(dataset$Count))
    )
    expect_identical(
      imputed@Minimum,
      ""
    )

    expect_is(
      imputed <- impute(model, dataset, minimum = "Bottom"),
      "rawImputed"
    )
    expect_identical(
      imputed@Minimum,
      "Bottom"
    )

    expect_error(
      impute(model, dataset, minimum = "Junk"),
      "object@Data does not have.*name.*Junk"
    )

    if (!require(INLA)) {
      skip("INLA package not available")
    }
    model <- inla(
      Count ~ Year + factor(Period) + factor(Site),
      data = dataset,
      family = "nbinomial",
      control.compute = list(config = TRUE)
    )
    expect_is(
      imputed <- impute(model, dataset, minimum = "Bottom"),
      "rawImputed"
    )

  })





  it("is robust for wrong imput", {
    expect_error(
      impute(model = "junk"),
      "can't handle a model of class character at this moment."
    )
    model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
    expect_error(
      impute(model = model, data = "junk"),
      "data does not inherit from class data.frame"
    )
    expect_error(
      impute(model = model, data = dataset, n_imp = -1),
      "n_imp is not a count"
    )
    expect_error(
      impute(model = model, data = dataset, n_imp = 0),
      "n_imp is not a count"
    )
    expect_error(
      impute(model = model, data = dataset, n_imp = "junk"),
      "n_imp is not a count"
    )
    wrong_dataset <- dataset
    wrong_dataset$Year <- NULL
    expect_error(
      impute(model = model, data = wrong_dataset),
      "object 'Year' not found"
    )
    wrong_dataset <- dataset
    wrong_dataset$Count <- NULL
    expect_error(
      impute(model = model, data = wrong_dataset),
      "data does not have.*name.*Count"
    )

    if (!require(INLA)) {
      skip("INLA package not available")
    }
    model <- INLA::inla(
      Mu ~ factor(Year) + factor(Period) + f(Site, model = "iid"),
      data = dataset,
      family = "nbinomial"
    )
    expect_error(
      impute(model),
"model must be fit with the 'config = TRUE' argument of control.compute"
    )
    model <- INLA::inla(
      Count ~ factor(Year) + factor(Period) + f(Site, model = "iid"),
      data = dataset,
      family = "binomial",
      Ntrials = max(dataset$Count, na.rm = TRUE),
      control.compute = list(config = TRUE),
      control.predictor = list(link = 1)
    )
    expect_error(
      impute(model),
      "Imputations from the 'binomial' family not yet defined"
    )

    model <- lme4::glmer(
      Count ~ factor(Year) + fPeriod + (1 | fSite),
      data = dataset,
      family = poisson
    )
    expect_error(
      impute(model, dataset),
      "impute can't handle factor"
    )
    expect_error(
      impute(model = model, data = "junk"),
      "data does not inherit from class data.frame"
    )
    expect_error(
      impute(model = model, data = dataset, n_imp = -1),
      "n_imp is not a count"
    )
    expect_error(
      impute(model = model, data = dataset, n_imp = 0),
      "n_imp is not a count"
    )
    expect_error(
      impute(model = model, data = dataset, n_imp = "junk"),
      "n_imp is not a count"
    )

    model <- lme4::glmer(
      Count ~ fYear + fPeriod + (1 | fSite),
      data = dataset,
      family = poisson
    )
    wrong_dataset <- dataset
    wrong_dataset$fPeriod <- NULL
    expect_error(
      impute(model = model, data = wrong_dataset),
      "object 'fPeriod' not found"
    )
    wrong_dataset <- dataset
    wrong_dataset$Count <- NULL
    expect_error(
      impute(model = model, data = wrong_dataset),
      "data does not have.*name.*Count"
    )
  })


  dataset <- generate_data(
    n_year = 10,
    n_site = 50,
    n_run = 1,
    year.factor = TRUE,
    period.factor = TRUE,
    site.factor = TRUE
  )
  dataset$Count[sample(nrow(dataset), 50)] <- NA
  dataset$Bottom <- 10000
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
      imputed <- impute(model, dataset, n_imp = n_imp),
      "rawImputed"
    )
    expect_identical(
      ncol(imputed@Imputation),
      n_imp
    )
    expect_identical(
      nrow(imputed@Imputation),
      sum(is.na(dataset$Count))
    )
    expect_identical(
      imputed@Minimum,
      ""
    )

    expect_is(
      imputed <- impute(model, dataset, minimum = "Bottom"),
      "rawImputed"
    )
    expect_identical(
      imputed@Minimum,
      "Bottom"
    )

    expect_error(
      impute(model, dataset, minimum = "Junk"),
      "object@Data does not have.*name.*Junk"
    )
  })
})
