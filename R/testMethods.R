#' test imputation methods
#'
#' @param dataset The dataset to test the methods
#' @importFrom MASS glm.nb
#' @importFrom geepack geeglm
#' @export
testMethods <- function(dataset){
  ### Fooling R CMD Check ###
  Site <- NULL
  Wave <- NULL
  rm(Site, Wave)
  ### Fooling R CMD Check ###

  n.year <- length(unique(dataset$Year))
  n.period <- length(unique(dataset$Period))
  initial <- round(exp(coef(glm.nb(Observed ~ 1, data = dataset))))
  test <- imputeUnderhill(
    data = dataset,
    formula = Observed ~ Year + Period + Site,
    initial = 0
  )
  model <- glm(
    Observed ~ 0 + Year + Period,
    data = aggregate(
      Observed ~ Year + Period,
      data = test$data,
      FUN = sum
    ),
    family = quasipoisson
  )
  estimate <- data.frame(
    Type = "UH0",
    coefficients(summary(model))[, 1:2]
  )

  test <- imputeUnderhill(
    data = dataset,
    formula = Observed ~ Year + Period + Site,
    initial = initial
  )
  model <- glm(
    Observed ~ 0 + Year + Period,
    data = aggregate(
      Observed ~ Year + Period,
      data = test$data,
      FUN = sum
    ),
    family = quasipoisson
  )
  estimate <- rbind(
    estimate,
    data.frame(
      Type = "UHM",
      coefficients(summary(model))[, 1:2]
    )
  )


  test <- imputeUnderhillAltered(
    data = dataset,
    formula = Observed ~ Year + Period + Site,
    initial = 0
  )
  model <- glm(
    Observed ~ 0 + Year + Period,
    data = aggregate(
      Observed ~ Year + Period,
      data = test$data,
      FUN = sum
    ),
    family = quasipoisson
  )
  estimate <- rbind(
    estimate,
    data.frame(
      Type = "UA0",
      coefficients(summary(model))[, 1:2]
    )
  )

  test <- imputeUnderhillAltered(
    data = dataset,
    formula = Observed ~ Year + Period + Site,
    initial = initial
  )
  model <- glm(
    Observed ~ 0 + Year + Period,
    data = aggregate(
      Observed ~ Year + Period,
      data = test$data,
      FUN = sum
    ),
    family = quasipoisson
  )
  estimate <- rbind(
    estimate,
    data.frame(
      Type = "UAM",
      coefficients(summary(model))[, 1:2]
    )
  )

  colnames(estimate)[3] <- "Std.err"
  observed.site <- length(unique(dataset$Site[!is.na(dataset$Observed)]))
  model <- geeglm(
    Observed ~  0 + Year + Period,
    id = Site,
    waves = Wave,
    data = dataset,
    family = poisson
  )
  estimate <- rbind(
    estimate,
    data.frame(
      Type = "GEE",
      coefficients(summary(model))[, 1:2] +
      log(c(
        rep(observed.site, n.year),
        rep(1, n.period - 1)
      ))
    )
  )

  model <- geeglm(
    Observed ~  Year + Period,
    id = Site,
    waves = Wave,
    data = dataset,
    family = poisson,
    corstr = "ar1"
  )
  estimate <- rbind(
    estimate,
    data.frame(
      Type = "GAR",
      coefficients(summary(model))[, 1:2] +
        log(c(
          rep(observed.site, n.year),
          rep(1, n.period - 1)
        ))
    )
  )


  rownames(estimate) <- NULL
  colnames(estimate)[3] <- "SE"
  estimate$Parameter <- c(
    paste("Year", seq_len(n.year), sep = ""),
    paste("Period", seq_len(n.period)[-1], sep = "")
  )

  test <- imputedTotals(
    data = dataset,
    imputations = imputeINLA(
      data = dataset,
      formula = Observed ~ Year + Period + f(Site, model = "iid")
    ),
    variable = "Observed",
    rhs = "Year + Period"
  )
  tmp <- summarizeImputationGLM(data = test, rhs = "0 + Year + Period")
  colnames(tmp)[2] <- "Estimate"
  tmp$Type <- "MI"
  rbind(estimate, tmp)
}
