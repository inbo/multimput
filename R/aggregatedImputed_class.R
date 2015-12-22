#' The aggregatedimputed class
#' Holds an aggregated imputation data set
#' @section Slots:
#'   \describe{
#'    \item{\code{Covariate}}{A data.frame with the covariates}
#'    \item{\code{Imputation}}{a matrix wih aggregated imputed values}
#'   }
#' @name aggregatedImputed-class
#' @rdname aggregatedImputed-class
#' @exportClass aggregatedImputed
#' @aliases aggregatedImputed-class
#' @importFrom methods setClass
#' @docType class
#' @include rawImputed_class.R
setClass(
  Class = "aggregatedImputed",
  representation = representation(
    Covariate = "data.frame",
    Imputation = "matrix"
  )
)
