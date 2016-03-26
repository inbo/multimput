#' impute missing data using the predicted values
#' @param data a \code{data.frame} containing the variables \code{Year}, \code{Month}, \code{Site} and \code{Observed}. The missing values of \code{Observed} are imputed by the algorithm.
#' @param formula A formula defining the model to use for the imputation
#' @export
#' @return A matrix with one row for each missing value. Each column is on imputation.
#' @template deprecated

imputeINLAfit <- function(
  data,
  formula = Observed ~ Year + Month + f(Site, model = "iid")
){
  # nocov start
  .Deprecated(
    new = "impute"
  )
  missing.data <- which(is.na(data[, as.character(formula[2])]))
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("the INLA package is required for this function")
  }
  model <- INLA::inla(
    formula,
    data = data,
    family = "nbinomial",
    control.predictor = list(compute = TRUE, link = 1)
  )
  data$Observed[missing.data] <-
    model$summary.fitted.values[missing.data, "mean"]
  return(data)
  # nocov end
}
