#' Calculate the multiple imputation average over some imputation sets
#' @param data a \code{data.frame} with the imputations and the covariates
#' @param model.fun the function to apply on each imputation set
#' @param rhs the right hand side of the model
#' @param model.args an optional list of arguments to pass to the model function
#' @param extractor a function which return a \code{matrix} or \code{data.frame}. The first column should contain the estimate, the second the standard error of the estimate
#' @param extractor.args an optional list of arguments to pass to the extractor function
#' @export
average_imputation <- function(data, model.fun, rhs, model.args, extractor, extractor.args){
  model.args <- c(list(data = data), model.args)
  raw.coef <- lapply(
    grep("^Imputation[[:digit:]]*$", colnames(data)),
    function(i){
      form <- as.formula(paste(colnames(data[i]), rhs, sep = "~"))
      model <- do.call(model.fun, c(form, model.args))
      do.call(extractor, c(list(model), extractor.args))
    }
  )
  estimate <- sapply(raw.coef, function(x){x[, 1]})
  variance <- sapply(raw.coef, function(x){x[, 2] ^ 2})
  cbind(
    Estimate = rowMeans(estimate),
    SE = sqrt(
      rowMeans(variance) + (1 + 1 / ncol(estimate)) * diag(var(t(estimate)))
    )
  )
}
