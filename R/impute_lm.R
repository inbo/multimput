#' @rdname impute
#' @importFrom methods setMethod
#' @param data The dataset holding both the observed and the missing values
setMethod(
  f = "impute",
  signature = signature(model = "lm"),
  definition = function(model, data, ..., n.imp = 19){
    response <- as.character(terms(model))[2]
    missing.obs <- which(is.na(data[, response]))
    prediction <- predict(model, newdata = data[missing.obs, ], se.fit = TRUE)
    prediction$se.pred <- sqrt(
      prediction$residual.scale ^ 2 + prediction$se.fit ^ 2
    )
    rt.value <- matrix(
      rt(length(missing.obs) * n.imp, df = prediction$df),
      ncol = n.imp
    )
    new(
      "rawImputed",
      Model = model,
      Data = data,
      Imputation = prediction$fit + rt.value * prediction$se.pred
    )
  }
)
