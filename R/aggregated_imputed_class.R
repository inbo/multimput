#' The aggregatedImputed class
#' Holds an aggregated imputation data set
#' @section Slots:
#'   \describe{
#'    \item{`Covariate`}{A data.frame with the covariates.}
#'    \item{`Imputation`}{A matrix wih aggregated imputed values.}
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
