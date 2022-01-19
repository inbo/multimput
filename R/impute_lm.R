#' @rdname impute
#' @importFrom methods new setMethod
#' @importFrom assertthat assert_that is.count has_name
#' @importFrom stats terms predict rt
#' @param data The dataset holding both the observed and the missing values
#' @examples
#' dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
#' dataset$Count[sample(nrow(dataset), 50)] <- NA
#' model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
#' impute(model, dataset)
#' @include impute_generic.R
setMethod(
  f = "impute",
  signature = signature(model = "lm"),
  definition = function(model, data, ..., n_imp) {
    check_old_names(..., old_names = c(n_imp = "n.imp"))
    assert_that(inherits(data, "data.frame"))
    assert_that(is.count(n_imp))

    response <- as.character(terms(model))[2]
    assert_that(has_name(data, response))

    missing_obs <- which(is.na(data[, response]))
    prediction <- predict(model, newdata = data[missing_obs, ], se.fit = TRUE)
    prediction$se.pred <- sqrt(
      prediction$residual.scale ^ 2 + prediction$se.fit ^ 2
    )
    rt_value <- matrix(
      rt(length(missing_obs) * n_imp, df = prediction$df),
      ncol = n_imp
    )
    dots <- list(...)
    if (is.null(dots$minimum)) {
      dots$minimum <- ""
    }
    new(
      "rawImputed",
      Data = data,
      Response = response,
      Imputation = prediction$fit + rt_value * prediction$se.pred,
      Minimum = dots$minimum
    )
  }
)
