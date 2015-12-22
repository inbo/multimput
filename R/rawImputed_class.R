#' The rawimputed class
#' Holds a dataset and imputed values
#' @section Slots:
#'   \describe{
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
    Data = "data.frame",
    Response = "character",
    Imputation = "matrix"
  )
)
