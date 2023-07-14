#' The `aggregatedImputed` class
#' Holds an aggregated imputation data set
#' @section Slots:
#'   \describe{
#'    \item{`Covariate`}{A data.frame with the covariates.}
#'    \item{`Imputation`}{A matrix with aggregated imputed values.}
#'   }
#' @name aggregatedImputed-class
#' @rdname aggregatedImputed-class
#' @exportClass aggregatedImputed
#' @aliases aggregatedImputed-class
#' @importFrom methods setClass
#' @docType class
#' @include raw_imputed_class.R
setClass(
  Class = "aggregatedImputed",
  representation = representation(
    Covariate = "data.frame",
    Imputation = "matrix"
  )
)

#' @importFrom methods setValidity
setValidity(
  "aggregatedImputed",
  function(object) {
    stopifnot(
      "`Imputation` must be numeric" = is.numeric(object@Imputation),
      "`Imputation` must contain at least two columns" =
        ncol(object@Imputation) > 1,
      "`Covariate` must have the same number of rows as `Imputation`" =
        nrow(object@Imputation) == nrow(object@Covariate),
      "colnames(`Imputation`) must have the format 'Imputation0000'" =
        all(grepl("^Imputation\\d{4}$", colnames(object@Imputation)))
    )
  }
)
