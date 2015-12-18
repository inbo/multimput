#' impute missing data using multiple imputation
#' @param data a \code{data.frame} containing the variables \code{Year}, \code{Month}, \code{Site} and \code{Observed}. The missing values of \code{Observed} are imputed by the algorithm.
#' @param formula A formula defining the model to use for the imputation
#' @param n.sim the number of simulations
#' @param family "nbinomial" for a negative binomial distribution or "poisson" for a Poisson distribution
#' @export
#' @return A matrix with one row for each missing value. Each column is on imputation.

imputeINLA <- function(
  data,
  formula = Observed ~ Year + Month + f(Site, model = "iid"),
  n.sim = 499,
  family = c("nbinomial", "poisson")
){
  family <- match.arg(family)
  missing.data <- which(is.na(data[, as.character(formula[2])]))
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("the INLA package is required for this function")
  }
  model <- INLA::inla(
    formula,
    data = data,
    family = family,
    control.predictor = list(compute = TRUE)
  )
  parameters <- model$summary.fitted.values[missing.data, c("mean", "sd")]
  mu <- replicate(
    n.sim,
    rnorm(nrow(parameters), mean = parameters$mean, sd = parameters$sd)
  )
  if (family == "nbinomial") {
    size <- INLA::inla.hyperpar.sample(n = n.sim, result = model)[, 1]
    sapply(seq_len(n.sim), function(i){
      rnbinom(nrow(mu), mu = exp(mu[, i]), size = size[i])
    })
  } else {
    sapply(seq_len(n.sim), function(i){
      rpois(nrow(mu), lambda = exp(mu[, i]))
    })
  }
}
