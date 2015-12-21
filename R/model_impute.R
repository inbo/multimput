#' Model an imputed dataset
#' @param object the imputed dataset
#' @param model.fun the function to apply on each imputation set
#' @param rhs the right hand side of the model
#' @param model.args an optional list of arguments to pass to the model function.
#' @param extractor a function which return a \code{matrix} or \code{data.frame}. The first column should contain the estimate, the second the standard error of the estimate
#' @param extractor.args an optional list of arguments to pass to the extractor function.
#' @name model_impute
#' @rdname model_impute
#' @exportMethod model_impute
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "model_impute",
  def = function(
    object,
    model.fun,
    rhs,
    model.args,
    extractor,
    extractor.args
){
    standard.generic("model_impute") # nocov
  }
)

#' @rdname model_impute
#' @importFrom methods setMethod
setMethod(
  f = "model_impute",
  signature = signature(object = "ANY"),
  definition = function(
    object,
    model.fun,
    rhs,
    model.args,
    extractor,
    extractor.args
  ){
    stop("model_impute() doesn't handle a '", class(object), "' object")
  }
)

#' @rdname model_impute
#' @importFrom methods setMethod
#' @examples
#' dataset <- generateData(n.year = 10, n.site = 50, n.run = 1)
#' dataset$Count[sample(nrow(dataset), 50)] <- NA
#' model <- lm(Count ~ Year + factor(Period) + factor(Site), data = dataset)
#' imputed <- impute(data = dataset, model = model)
#' aggr <- aggregate_impute(imputed, grouping = c("Year", "Period"), fun = sum)
#' extractor <- function(model){
#'   summary(model)$coefficients[, c("Estimate", "Std. Error")]
#' }
#' model_impute(
#'   object = aggr,
#'   model.fun = lm,
#'   rhs = "0 + factor(Year)",
#'   extractor = extractor
#' )
#' @include aggregatedImputed_class.R
setMethod(
  f = "model_impute",
  signature = signature(object = "aggregatedImputed"),
  definition = function(
    object,
    model.fun,
    rhs,
    model.args,
    extractor,
    extractor.args
  ){
    if (missing(model.args)) {
      model.args <- list()
    }
    if (missing(extractor.args)) {
      extractor.args <- list()
    }
    form <- as.formula(paste("Imputed", rhs, sep = "~"))
    raw.coef <- lapply(
      seq_len(ncol(object@Imputation)),
      function(i){
        data <- cbind(
          Imputed = object@Imputation[, i],
          object@Covariate
        )
        model.args <- c(list(data = data), model.args)
        model <- do.call(model.fun, c(form, model.args))
        do.call(extractor, c(list(model), extractor.args))
      }
    )
    estimate <- sapply(
      raw.coef,
      function(x){
        x[, 1]
      }
    )
    variance <- sapply(
      raw.coef,
      function(x){
        x[, 2] ^ 2
      }
    )
    cbind(
      Estimate = rowMeans(estimate),
      SE = sqrt(
        rowMeans(variance) + (1 + 1 / ncol(estimate)) * diag(var(t(estimate)))
      )
    )
  }
)
