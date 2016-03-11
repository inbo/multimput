#' impute missing data using multiple imputation
#' @param data a \code{data.frame} containing the variables \code{Year}, \code{Month}, \code{Site} and \code{Observed}. The missing values of \code{Observed} are imputed by the algorithm.
#' @param size the size of the negative binomial distribution
#' @param mean the name of the variable in \code{data}, holding the true mean
#' @param respons the name of the variable in \code{data}, holding the observed values
#' @param n.sim the number of simulations
#' @export
#' @return A matrix with one row for each missing value. Each column is on imputation.
#' @template deprecated

imputeTruth <- function(
  data,
  size,
  mean = "Mu",
  respons = "Observed",
  n.sim = 499
){
  # nocov start
  .Deprecated(
    new = "impute"
  )
  missing.data <- which(is.na(data[, respons]))
  sapply(seq_len(n.sim), function(i){
    rnbinom(length(missing.data), mu = data[missing.data, mean], size = size)
  })
  # nocov end
}
