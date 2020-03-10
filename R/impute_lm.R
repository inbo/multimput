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
  definition = function(model, data, ..., n.imp) {
    assert_that(inherits(data, "data.frame"))
    assert_that(is.count(n.imp))

    response <- as.character(terms(model))[2]
    assert_that(has_name(data, response))

    missing.obs <- which(is.na(data[, response]))
    prediction <- predict(model, newdata = data[missing.obs, ], se.fit = TRUE)
    prediction$se.pred <- sqrt(
      prediction$residual.scale ^ 2 + prediction$se.fit ^ 2
    )
    rt.value <- matrix(
      rt(length(missing.obs) * n.imp, df = prediction$df),
      ncol = n.imp
    )
    dots <- list(...)
    if (is.null(dots$minimum)) {
      dots$minimum <- ""
    }
    new(
      "rawImputed",
      Data = data,
      Response = response,
      Imputation = prediction$fit + rt.value * prediction$se.pred,
      Minimum = dots$minimum
    )
  }
)
