#' impute missing data using an alterned Underhill method
#' @param data a \code{data.frame} containing the variables \code{Year}, \code{Month}, \code{Site} and \code{Observed}. The missing values of \code{Observed} are imputed by the algorithm.
#' @param formula A formula defining the model to use for the imputation
#' @param initial the initial value by which the missing values are replaced
#' @export
#' @return A list with two elements: \code{data} with imputed values and \code{iterations} which is the number of iterations
#' @importFrom MASS glm.nb

imputeUnderhillAltered <- function(
  data,
  formula = Observed ~ Year + Month + Site,
  initial = 0
){
  missing.data <- which(is.na(data[, as.character(formula[2])]))
  data$Observed[missing.data] <- initial
  do.loop <- TRUE
  iterations <- 1
  while (do.loop) {
    model <- glm.nb(formula, data = data)
    new.values <- round(
      predict(model, newdata = data[missing.data, ], type = "response")
    )
    if (any(data[missing.data, as.character(formula[2])] != new.values)) {
      data[missing.data, as.character(formula[2])] <- new.values
      iterations <- iterations + 1
    } else {
      break
    }
  }
  return(list(data = data, iterations = iterations))
}
