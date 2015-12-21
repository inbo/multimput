#' @importFrom methods setClassUnion
#' @include import_S3_classes.R
setClassUnion("imputeModel", c("inla", "lm"))

#' The rawimputed class
#' Holds a dataset, an imputation model and imputed values
#' @section Slots:
#'   \describe{
#'    \item{\code{Model}}{The imputation model}
#'    \item{\code{Data}}{a data.frame with the data}
#'    \item{\code{Response}}{a character holding the name of the response variable}
#'    \item{\code{Imputation}}{a matrix wih imputed values}
#'   }
#' @name rawImputed-class
#' @rdname rawImputed-class
#' @exportClass rawImputed
#' @aliases rawImputed-class
#' @importFrom methods setClass
#' @docType class
setClass(
  Class = "rawImputed",
  representation = representation(
    Model = "imputeModel",
    Data = "data.frame",
    Response = "character",
    Imputation = "matrix"
  )
)
