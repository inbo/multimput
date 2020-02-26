#' The rawimputed class
#' Holds a dataset and imputed values
#' @section Slots:
#'   \describe{
#'    \item{`Data`}{A data.frame with the data.}
#'    \item{`Response`}{A character holding the name of the response variable.}
#'    \item{`Minimum`}{An optional character holding the name of the variable
#'    with the minimum.}
#'    \item{`Imputation`}{A matrix wih imputed values.}
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
    Minimum = "character",
    Imputation = "matrix"
  )
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that is.string
setValidity(
  "rawImputed",
  function(object) {
    assert_that(is.string(object@Response))
    assert_that(has_name(object@Data, object@Response))

    assert_that(length(object@Minimum) == 1)
    if (!is.na(object@Minimum) && object@Minimum != "") {
      assert_that(has_name(object@Data, object@Minimum))
    }
  }
)
